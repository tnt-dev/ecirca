-module(ecirca).
-compile({parse_transform, ecirca_pt}).
-include("ecirca.hrl").

%% Init
-export([new/4, new/3, new/2]).
%% Getters
-export([get/2, slice/3]).
%% Setters
-export([set/3, update/3, push/2, push_many/3, push_list/2]).
%% Compile-time constants
-export([max_slice/0, max_size/0]).
%% Current circa properties
-export([size/1]).
%% Persistence
-export([load/2, load/1, save/1]).

-export_types([res/0, maybe_value/0, value/0]).

%% NOTE: changing of ecirca internal format will break macroses in ecirca.hrl
-opaque ecirca()          :: {ecirca, reference(), resource(),
                              pid(), ecirca_value_size()}.
-type resource()          :: <<>>.
-type value()             :: non_neg_integer().
-type maybe_value()       :: value() | empty.
-type nonneg()            :: non_neg_integer().
-type ecirca_type()       :: last | max | min | avg | sum.
-type ecirca_value_size() :: small | medium | large.
-type ecirca_atom_specs() :: [{atom(), strong | weak}].

%% @doc Returns new ecirca. Takes size and type only, assumes medium
%%      value size
-spec new(pos_integer(), ecirca_type()) -> {ok, ecirca()} |
                                           {error, max_size}.
new(Size, Type) -> new(Size, Type, medium).

%% @doc Returns new ecirca. Takes size, type and value size.
%%      Assumes empty atom spec list.
-spec new(pos_integer(),
          ecirca_type(),
          ecirca_value_size()) -> {ok, ecirca()} |
                                  {error, max_size}.
new(Size, Type, ValueSize) ->
    new(Size, Type, ValueSize, []).

%% @doc Returns new ecirca. Takes size, type and value size
-spec new(pos_integer(),
          ecirca_type(),
          ecirca_value_size(),
          ecirca_atom_specs()) -> {ok, ecirca()} |
                                  {error, max_size}.
new(Size, Type, small, AtomSpecs) ->
    make_ecirca(ecirca_small:new(Size, Type, AtomSpecs), small);
new(Size, Type, medium, AtomSpecs) ->
    make_ecirca(ecirca_medium:new(Size, Type, AtomSpecs), medium);
new(Size, Type, large, AtomSpecs) ->
    make_ecirca(ecirca_large:new(Size, Type, AtomSpecs), large).

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
-spec push(ecirca(), maybe_value()) -> ok | {error, overflow} |
                                       {error, unknown_atom}.
?WITH_ECIRCA(push, Value).

%% @doc Push a value to ecirca N times
%% TODO: all
-spec push_many(ecirca(), nonneg(), maybe_value()) -> ok.
push_many(Ecirca, N, Val) ->
    [push(Ecirca, Val) || _ <- lists:seq(1, N)],
    ok.

%% @doc Push a list to ecirca
%% TODO: all
-spec push_list(ecirca(), [maybe_value()]) -> ok.
push_list(Ecirca, Lst) ->
    [push(Ecirca, X) || X <- Lst],
    ok.

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
-spec max_size() -> {ok, pos_integer()}.
max_size() -> ecirca_medium:max_size().

%% @doc Returns max allowed size of slice
-spec max_slice() -> {ok, pos_integer()}.
max_slice() -> ecirca_medium:max_slice().

%% @doc Returns a size of ecirca
-spec size(ecirca()) -> {ok, pos_integer()}.
?WITH_ECIRCA(size).

%% @doc Loads ecirca from binary, assumes medium size of value
-spec load(binary()) -> {ok, ecirca()} |
                        {error, wrong_ecirca_value_type} |
                        {error, bad_binary} |
                        {error, max_size}.
load(Binary) -> load(Binary, medium).

%% @doc Loads ecirca from binary
-spec load(binary(),
           ecirca_value_size()) -> {ok, ecirca()} |
                                   {error, wrong_ecirca_value_type} |
                                   {error, bad_binary} |
                                   {error, max_size}.
load(Binary, small) ->
    make_ecirca(ecirca_small:load(Binary), small);
load(Binary, medium) ->
    make_ecirca(ecirca_medium:load(Binary), medium);
load(Binary, large) ->
    make_ecirca(ecirca_large:load(Binary), large).

%% @doc Saves ecirca to binary
-spec save(ecirca()) -> {ok, binary()}.
?WITH_ECIRCA(save).

%%% Internal functions

-spec make_ecirca(resource(), ecirca_value_size()) -> ecirca().
make_ecirca(Res, ValueSize) ->
    {ok, {ecirca, make_ref(), Res, self(), ValueSize}}.
