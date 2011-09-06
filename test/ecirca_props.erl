-module(ecirca_props).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-define(SERVER, ecirca).
-define(TYPES, [last, max, min, avg, sum]).
-define(VALUE_SIZES, [small, medium, large]).
%max_value(small)  -> 4096;
%max_value(medium) -> 134217728;
%max_value(large)  -> 576460752303423487.

-record(ecirca_model, {elements   :: array(),
                       size       :: pos_integer(),
                       value_size :: ecirca:ecirca_value_size(),
                       type       :: ecirca:ecirca_type()}).

ecirca_type() -> elements(?TYPES).
value_size() -> elements(?VALUE_SIZES).
ecirca_size() ->
    {ok, M} = ecirca:max_size(),
    proper_types:integer(0, M).
slice_size() ->
    {ok, M} = ecirca:slice_size(),
    proper_types:integer(0, M).
ecirca_value() -> proper_types:union([proper_types:integer(),
                                      empty]).
ecirca(State) -> elements(ets:tab2list(State)).

command(S) ->
    Ecircas = (ets:tab2list(S) =/= []),
    oneof([{call, ?SERVER, new, [ecirca_size(), ecirca_type(), value_size()]},
           {call, ?SERVER, new, [ecirca_size(), ecirca_type()]}]
          ++ [{call, ?MODULE, get, [ecirca(S), ecirca_size()]}
              || Ecircas]
          ++ [{call, ?MODULE, set, [ecirca(S), ecirca_size(), ecirca_value()]}
              || Ecircas]).

initial_state() -> ets:new(ecircas, []).

next_state(S, V, {call, _, new, [_, _]}=R) ->
    next_state(S, V, R);
next_state(S, V, {call, _, new, [Size, _Type, ValueSize]}) ->
    ets:insert(S, {make_ref(), V, #ecirca_model{elements   = array:new(),
                                                size       = Size,
                                                value_size = ValueSize}}),
    S;
next_state(S, _V, {call, _, get, _}) -> S;
next_state(S, _V, {call, _, set, [{Ref, _, Model}, Pos, Val]}) ->
    OldArray = Model#ecirca_model.elements,
    NewArray = array:set(Pos, Val, OldArray),
    NewModel = Model#ecirca_model{elements = NewArray},
    ets:update_element(S, Ref, {3, NewModel}),
    S.

precondition(_S, {call, _, get, [{_, _, Model}, Pos]}) ->
    Pos =< Model#ecirca_model.size;
precondition(_, _) -> true.

postcondition(_, _, _) -> true.


prop_server_works_fine() ->
    ?FORALL(Cmds, commands(?MODULE),
            begin
                {_,_,Result} = run_commands(?MODULE, Cmds),
                Result =:= ok
            end).

get({_, T, _}) -> ?SERVER:get(T).
set({_, T, _}) -> ?SERVER:set(T).

proper_test_() ->
    {timeout, 10,
     ?_assertEqual([], proper:module(ecirca_props, [{to_file, user}]))}.
