-module(ecirca).
-compile({parse_transform, ecirca_pt}).
-include("ecirca.hrl").

%% Init
-export([new/3]).
%% Getters
-export([get/2, slice/3]).
%% Setters
-export([set/3, update/3, push/2, push_many/3, push_list/2]).
%% Compile-time constants
-export([max_slice/1, max_size/1]).
%% Current circa properties
-export([size/1]).
%% Persistence
-export([load/2, save/1]).

-export_types([res/0, maybe_value/0, value/0]).

%% NOTE: changing of ecirca internal format will break parse transform
-opaque ecirca()          :: {ecirca, resource(),
                              pid(), ecirca_value_size()}.
-type resource()          :: <<>>.
-type value()             :: non_neg_integer().
-type maybe_value()       :: value() | empty.
-type nonneg()            :: non_neg_integer().
-type ecirca_type()       :: last | max | min | avg | sum.
-type ecirca_value_size() :: small | medium | large.

%% @doc Returns new ecirca. Takes size and type
-spec new(pos_integer(),
          ecirca_type(),
          ecirca_value_size()) -> {ok, ecirca()} |
                                  {error, max_size}.
new(Size, Type, small) ->
    {ok, {ecirca, ecirca_small:new(Size, Type), self(), small}};
new(Size, Type, medium) ->
    {ok, {ecirca, ecirca_medium:new(Size, Type), self(), medium}};
new(Size, Type, large) ->
    {ok, {ecirca, ecirca_large:new(Size, Type), self(), large}}.

%% @doc Sets a value in ecirca. Returns {old value, new value} tuple
-spec set(ecirca(), pos_integer(), maybe_value()) -> {ok, {maybe_value(),
                                                           maybe_value()}}.
?WITH_ECIRCA(set, Position, Value).

%% @doc Updates a value in ecirca, action is defined by type of ecirca.
%%      Returns {old value, new value} tuple
-spec update(ecirca(), pos_integer(), maybe_value()) -> {ok, {maybe_value(),
                                                              maybe_value()}}.
?WITH_ECIRCA(update, Position, Value).

%% @doc Push a value to ecirca
-spec push(ecirca(), maybe_value()) -> ok | {error, overflow}.
?WITH_ECIRCA(push, Value).

%% @doc Push a value to ecirca N times
%% TODO: all
-spec push_many(ecirca(), nonneg(), maybe_value()) -> ok.
push_many(Res, N, Val) ->
    [push(Res, Val) || _ <- lists:seq(1, N)],
    ok.

%% @doc Push a list to ecirca
%% TODO: all
-spec push_list(ecirca(), [maybe_value()]) -> ok.
push_list(_Ecirca, _Lst) -> ok.

%% @doc Returns a value
-spec get(ecirca(), pos_integer()) -> {ok, maybe_value()}.
?WITH_ECIRCA(get, Position).

%% @doc Returns a slice of ecirca
-spec slice(ecirca(),
            pos_integer(),
            pos_integer()) -> {ok, [maybe_value()]} |
                              {error, slice_too_big}.
?WITH_ECIRCA(slice, Start, End).

%% @doc Returns max allowed size of ecirca
-spec max_size(ecirca_value_size()) -> {ok, pos_integer()}.
?WITH_VALUE_SIZE(max_size).

%% @doc Returns max allowed size of slice
-spec max_slice(ecirca_value_size()) -> {ok, pos_integer()}.
?WITH_VALUE_SIZE(max_slice).

%% @doc Returns a size of ecirca
-spec size(ecirca()) -> {ok, pos_integer()}.
?WITH_ECIRCA(size).

%% @doc Loads ecirca from binary
-spec load(binary(),
           ecirca_value_size()) -> {ok, ecirca()} |
                                   {error, wrong_ecirca_value_type} |
                                   {error, bad_binary} |
                                   {error, max_size}.
load(Binary, small) ->
    {ok, {ecirca, ecirca_small:load(Binary), self(), small}};
load(Binary, medium) ->
    {ok, {ecirca, ecirca_medium:load(Binary), self(), medium}};
load(Binary, large) ->
    {ok, {ecirca, ecirca_large:load(Binary), self(), large}}.


%% @doc Saves ecirca to binary
-spec save(ecirca()) -> {ok, binary()}.
?WITH_ECIRCA(save).

