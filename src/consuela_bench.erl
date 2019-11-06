-module(consuela_bench).

-export([start/0]).

start() ->
    Opts = [],
    Configs = [
        "src/consuela.config"
    ],
    {ok, _} = application:ensure_all_started(?MODULE),
    ok = basho_bench:setup_benchmark(Opts, Configs),
    ok = basho_bench:run_benchmark(),
    ok = basho_bench:await_completion(infinity),
    init:stop().
