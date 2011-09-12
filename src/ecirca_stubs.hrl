-include("ecirca.hrl").
-compile(export_all).
-on_load(nif_init/0).

new(_, _, _)    -> ?STUB.
set(_, _, _)    -> ?STUB.
update(_, _, _) -> ?STUB.
push(_, _)      -> ?STUB.
get(_, _)       -> ?STUB.
slice(_, _, _)  -> ?STUB.
max_size()      -> ?STUB.
max_slice()     -> ?STUB.
size(_)         -> ?STUB.
save(_)         -> ?STUB.
load(_)         -> ?STUB.

%% @private
%% @doc Loads a NIF
-spec nif_init() -> ok | {error, _}.
nif_init() ->
    SoName = case code:priv_dir(?APPNAME) of
                 {error, bad_name} ->
                     case filelib:is_dir(filename:join(["..", priv])) of
                         true ->
                             filename:join(["..", priv, ?MODULE]);
                         _ ->
                             filename:join([priv, ?MODULE])
                     end;
                 Dir ->
                     filename:join(Dir, ?MODULE)
             end,
    erlang:load_nif(SoName, 1).

-spec not_loaded(pos_integer()) -> ok.
not_loaded(Line) -> exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
