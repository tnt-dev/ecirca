-module(ecirca_props).
-behaviour(proper_statem).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([initial_state/0, initial_state/3, command/1,
         precondition/2, postcondition/3, next_state/3]).

-define(SERVER, ecirca).
-define(TYPES, [last, max, min, avg, sum]).
-define(VALUE_SIZES, [small, medium, large]).
max_value(small)  -> 4096;
max_value(medium) -> 134217728;
max_value(large)  -> 576460752303423487.

-record(state, {ecirca     = undefined   :: ecirca:ecirca() | undefined,
                elements   = array:new() :: array(),
                previous   = none        :: pos_integer() | atom(),
                size       = 0           :: pos_integer(),
                value_size = medium      :: ecirca:ecirca_value_size(),
                type       = last        :: ecirca:ecirca_type()}).

ecirca_type() -> elements(?TYPES).
value_size() -> elements(?VALUE_SIZES).
ecirca_size(short) -> proper_types:integer(1, 1000);
ecirca_size(full) ->
    {ok, M} = ecirca:max_size(),
    proper_types:integer(1, M).
value(S) ->
    Max = max_value(S#state.value_size),
    proper_types:union([proper_types:integer(0, Max), empty]).
position(S) ->  proper_types:integer(1, S#state.size).
ecirca(S) -> S#state.ecirca.
push_count(S) -> proper_types:integer(1, S#state.size * 2).

initial_state() ->
    initial_state(1000, last, medium).

initial_state(Size, Type, ValueSize) ->
    #state{ecirca     = undefined,
           elements   = array:new([{default, empty}]),
           previous   = none,
           size       = Size,
           value_size = ValueSize,
           type       = Type}.

command(#state{ecirca = undefined}=S) ->
    {call, ?SERVER, new, [S#state.size, S#state.type, S#state.value_size]};
command(S) ->
    frequency(
      [{50, {call, ?SERVER, set, [ecirca(S), position(S), value(S)]}},
       {50, {call, ?SERVER, get, [ecirca(S), position(S)]}},
       {50, {call, ?SERVER, slice, [ecirca(S), position(S), position(S)]}},
       {50, {call, ?SERVER, push, [ecirca(S), value(S)]}},
       {50, {call, ?SERVER, push_many, [ecirca(S), push_count(S), value(S)]}},
       {10, {call, ?SERVER, size, [ecirca(S)]}},
       {5, {call, ?SERVER, max_slice, []}},
       {5, {call, ?SERVER, max_size, []}}]).

precondition(_S, {call, _, slice, [_, Start, End]}) ->
    abs(End - Start) =< ecirca:max_slice();
precondition(_, _) -> true.

next_state(S, V, {call, _Mod, new, _Args}) ->
    S#state{ecirca = {call, erlang, element, [2, V]}};
next_state(S, _V, {call, _Mod, set, [_, Pos, Val]}) ->
    S#state{elements = array:set(Pos - 1, Val, S#state.elements),
            previous = array:get(Pos - 1, S#state.elements)};
next_state(S, _V, {call, _Mod, push, [_, Val]}) ->
    Lst = [Val | array:to_list(S#state.elements)],
    LstTrimmed = lists:sublist(Lst, S#state.size),
    S#state{elements = array:from_list(LstTrimmed, empty)};
next_state(S, _V, {call, _Mod, push_many, [_, Count, Val]}) ->
    Lst = lists:duplicate(Count, Val) ++ array:to_list(S#state.elements),
    LstTrimmed = lists:sublist(Lst, S#state.size),
    S#state{elements = array:from_list(LstTrimmed, empty)};
next_state(S, _V, {call, _Mod, push_list, [_, PushLst]}) ->
    Lst = PushLst ++ array:to_list(S#state.elements),
    LstTrimmed = lists:sublist(Lst, S#state.size),
    S#state{elements = array:from_list(LstTrimmed, empty)};
next_state(S, _, _) -> S.

%% returned value should be equal to one that we've passed
postcondition(S, {call, _Mod, get, [_, Pos]}, Res) ->
    Res =:= {ok, array:get(Pos - 1, S#state.elements)};
%% returned ecirca should have properties that was requested
postcondition(_S, {call, _Mod, new, [Size, _Type, ValueSize]}, Res) ->
    {ok, Ecirca} = Res,
    {ecirca, _Ref, _Resource, Pid, ValueSize} = Ecirca,
    {ok, RealSize} = ecirca:size(Ecirca),
    (Size == RealSize andalso
     Pid == self());
%% slice should be right slice
postcondition(S, {call, _Mod, slice, [_, Pos1, Pos2]}, Res) ->
    {ok, Slice} = Res,
    Idxs = lists:seq(min(Pos1, Pos2), max(Pos1, Pos2)),
    PreRefSlice = [array:get(I-1, S#state.elements) || I <- Idxs],
    RefSlice = case Pos2 < Pos1 of
                   true  -> lists:reverse(PreRefSlice);
                   false -> PreRefSlice
               end,
    RefSlice =:= Slice;
%% value returned by set should be {ok, {OldValue, NewValue}}
postcondition(S, {call, _Mod, set, [_, Pos, Val]}, Res) ->
    Res =:= {ok, {array:get(Pos, S#state.elements), Val}};
postcondition(_S, {call, _Mod, push, [_, _]}, Res) ->
    Res =:= ok;
postcondition(S, {call, _Mod, size, [_]}, Res) ->
    {ok, Size} = Res,
    Size == S#state.size;
postcondition(_S, {call, _Mod, max_slice, []}, Res) ->
    {ok, SliceSize} = Res,
    is_integer(SliceSize);
postcondition(_S, {call, _Mod, max_size, []}, Res) ->
    {ok, MaxSize} = Res,
    is_integer(MaxSize);
postcondition(_, _, _) -> true.


prop_main() ->
    ?FORALL({Size, Type, ValueSize}, {ecirca_size(short),
                                      noshrink(ecirca_type()),
                                      noshrink(value_size())},
            ?FORALL(Cmds, commands(?MODULE,
                                   initial_state(Size, Type, ValueSize)),
                    begin
                        {H,S,Res} = run_commands(?MODULE, Cmds),
                        ?WHENFAIL(
                           io:format(user, "History: ~s\nState: ~s\nRes: ~p\n",
                                     [io_lib_pretty:print(H, 1, 80, 30),
                                      io_lib_pretty:print(S, 1, 80, 30),
                                      Res]),
                           aggregate(command_names(Cmds), Res =:= ok))
                    end)).

proper_test_() ->
    {timeout, 600,
     ?_assertEqual([], proper:module(ecirca_props, [{to_file, user},
                                                    {numtests, 1000}]))}.
