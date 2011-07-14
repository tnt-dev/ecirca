-module(ecirca_bench).
-export([start/0]).

start() ->
    {ok, T} = ecirca:new(100),
    Seq = lists:seq(1, 1000),
    timer:tc(fun() -> push1k(T, Seq) end).

push1k(Ecirca, Seq) ->
    [ecirca:push(Ecirca, X) || X <- Seq],
    ok.
