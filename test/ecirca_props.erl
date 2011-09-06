-module(ecirca_props).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

prop_testtt() ->
    ?FORALL(A, integer(), A == 1).

