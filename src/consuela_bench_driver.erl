-module(consuela_bench_driver).

-define(NS, <<"bench">>).

-export([
    new/1,
    terminate/2,
    run/4
]).

%%

-behaviour(gen_server).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

%%

-export([register_name/2]).
-export([unregister_name/1]).
-export([whereis_name/1]).

%%

-export([handle_beat/2]).

%% driver

new({single_worker, N, _}) ->
    Ref = list_to_atom("consuela" ++ integer_to_list(N)),
    {Host, Port} = pick_random(basho_bench_config:get(consul_endpoints)),
    {ok, Pid} = consuela_registry_sup:start_link(Ref, #{
        nodename  => Host,
        namespace => basho_bench_config:get(consuela_namespace, ?NS),
        shutdown  => basho_bench_config:get(shutdown_timeout, 5000),
        consul    => #{
            url   => "http://" ++ Host ++ ":" ++ integer_to_list(Port),
            opts  => genlib_map:compact(#{
                acl            => basho_bench_config:get(consul_acl_token),
                transport_opts => basho_bench_config:get(consul_client_transport_opts, #{})
                % pulse          => {?MODULE, {client, N}}
            })
        },
        registry  => #{
            pulse => {?MODULE, {registry, N}}
        },
        reaper    => #{
            pulse => {?MODULE, {reaper, N}}
        }
    }),
    {ok, #{
        registry => Ref,
        sup_pid  => Pid
    }}.

terminate(_Reason, #{sup_pid := Pid}) ->
    consuela_registry_sup:stop(Pid);
terminate(_Reason, _) ->
    ok.

run(spawn_register, NameGen, LifetimeGen, St = #{registry := Ref}) ->
    Name = NameGen(),
    Lifetime = basho_bench_config:get(minimum_lifetime) + LifetimeGen(),
    case gen_server:start({via, ?MODULE, {Ref, Name}}, ?MODULE, Lifetime, []) of
        {ok, Pid} when is_pid(Pid) ->
            {ok, St};
        {error, {already_started, _}} ->
            {ok, St};
        {error, Reason} ->
            {error, Reason, St}
    end;
run(lookup_registered, NameGen, _ValueGen, St = #{registry := Ref}) ->
    Name = NameGen(),
    case consuela_registry_server:lookup(Ref, Name) of
        {ok, Pid} when is_pid(Pid) ->
            {ok, St};
        {error, notfound} ->
            {ok, St}
    end.

%% registry wrapper

register_name({Ref, Name}, Pid) ->
    case consuela_registry_server:register(Ref, Name, Pid) of
        ok ->
            yes;
        {error, exists} ->
            no
    end.

unregister_name({Ref, Name}) ->
    ok = consuela_registry_server:unregister(Ref, Name, self()),
    ok.

whereis_name({Ref, Name}) ->
    case consuela_registry_server:lookup(Ref, Name) of
        {ok, Pid} ->
            Pid;
        {error, notfound} ->
            undefined
    end.

%% utilities

pick_random(L) ->
    lists:nth(rand:uniform(length(L)), L).

%% gen-server stub

init(Lifetime) ->
    {ok, _St = [], Lifetime}.

handle_call(Call, From, St) ->
    erlang:error({unexpected, {{call, From}, Call}, St}).

handle_cast(Cast, St) ->
    erlang:error({unexpected, {cast, Cast}, St}).

handle_info(timeout, St) ->
    {stop, shutdown, St}.

%% pulse

handle_beat(Beat, {Producer, WorkerId}) ->
    _ = handle_stat(Beat, Producer),
    lager:log(info, self(), "[~p] [~p] ~p", [WorkerId, Producer, Beat]).

handle_stat({{register, Reg}, started}, registry) ->
    started({reg, Reg});
handle_stat({{register, Reg}, {finished, Result}}, registry) ->
    basho_bench_stats:op_complete({register, register}, Result, elapsed({reg, Reg}));

handle_stat({{zombie, Reg}, enqueued}, reaper) ->
    started({reaping, Reg});
handle_stat({{zombie, Reg}, {reaping, succeeded}}, reaper) ->
    basho_bench_stats:op_complete({reaping, reaping}, ok, elapsed({reaping, Reg}));
handle_stat({{zombie, _Reg}, {reaping, {failed, Reason}}}, reaper) ->
    basho_bench_stats:op_complete({reaping, reaping}, {error, Reason}, 0);
handle_stat({{zombie, Reg}, {reaping, {skipped, _}}}, reaper) ->
    elapsed({reaping, Reg});

handle_stat(_, _) ->
    ok.

started(Key) ->
    erlang:put({started, Key}, now_us()).

elapsed(Key) ->
    erlang:max(0, now_us() - erlang:erase({started, Key})).

now_us() ->
    erlang:monotonic_time(microsecond).
