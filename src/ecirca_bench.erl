-module(ecirca_bench).
-export([start/0]).
-define(WITH_N(F), fun(N) -> fun() -> F end end).

start() ->
    io:format("PUSH ===========~n"),
    io:format("~nSmall ecirca ----------~n"),
    {ok, E1} = ecirca:new(100, last, small),
    bench("push 1", ?WITH_N(pushN(E1, 1, N))),
    bench("push 1000", ?WITH_N(pushN(E1, 1000, N))),
    bench("push MAX", ?WITH_N(pushN(E1, 4096, N))),
    io:format("~nMedium ecirca ----------~n"),
    {ok, E2} = ecirca:new(100, last, medium),
    bench("push 1", ?WITH_N(pushN(E2, 1, N))),
    bench("push 1000", ?WITH_N(pushN(E2, 1000, N))),
    bench("push MAX", ?WITH_N(pushN(E1, 134217728, N))),
    io:format("~nLarge ecirca ----------~n"),
    {ok, E3} = ecirca:new(100, last, large),
    bench("push 1", ?WITH_N(pushN(E3, 1, N))),
    bench("push 1000", ?WITH_N(pushN(E3, 1000, N))),
    bench("push MAX", ?WITH_N(pushN(E1, 576460752303423487, N))),
    ok.

bench(Name, Fun) ->
    bench(Name, Fun, [100, 10000, 1000000]).

bench(Name, Fun, Ns) when is_list(Ns) ->
    io:format("-------------~n"),
    [bench(Name, Fun, N) || N <- Ns];
bench(Name, Fun, N) when is_integer(N) ->
    {Time, _} = timer:tc(Fun(N)),
    io:format("~s runned ~p times, took ~p us (~p us each)~n",
              [Name, N, Time, Time / N]).

pushN(_, _, 0) -> ok;
pushN(Ecirca, Val, N) ->
    ecirca:push(Ecirca, Val),
    pushN(Ecirca, Val, N-1).

