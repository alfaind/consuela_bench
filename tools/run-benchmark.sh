#!/bin/bash

rebar3 compile
erl -pa _build/default/lib/*/ebin -s consuela_bench
