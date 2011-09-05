-define(APPNAME, ?MODULE).
-define(LIBNAME, ?MODULE).
-define(STUB, not_loaded(?LINE)).
-define(CHECK_PID(PID, DO), case self() == PID of
                                true -> DO;
                                false -> {error, cretor_only}
                            end).

-define(CLAUSE(NAME, TYPE),
        NAME({_, Res, Pid, TYPE}) ->
               ?CHECK_PID(Pid, (ecirca ++ TYPE):NAME(Res))).

-define(CLAUSE(NAME, TYPE, A),
        NAME({_, Res, Pid, TYPE}, A) ->
               ?CHECK_PID(Pid, (ecirca ++ TYPE):NAME(Res, A))).

-define(CLAUSE(NAME, TYPE, A, B),
        NAME({_, Res, Pid, TYPE}, A, B) ->
               ?CHECK_PID(Pid, (ecirca ++ TYPE):NAME(Res, A, B))).

-define(CLAUSE(NAME, TYPE, A, B, C),
        NAME({_, Res, Pid, TYPE}, A, B, C) ->
               ?CHECK_PID(Pid, (ecirca ++ TYPE):NAME(Res, A, B, C))).

-define(WITH_ECIRCA(NAME),
        ?CLAUSE(NAME, small);
        ?CLAUSE(NAME, medium);
        ?CLAUSE(NAME, large)).

-define(WITH_ECIRCA(NAME, A),
        ?CLAUSE(NAME, small, A);
        ?CLAUSE(NAME, medium, A);
        ?CLAUSE(NAME, large, A)).

-define(WITH_ECIRCA(NAME, A, B),
        ?CLAUSE(NAME, small, A, B);
        ?CLAUSE(NAME, medium, A, B);
        ?CLAUSE(NAME, large, A, B)).

-define(WITH_ECIRCA(NAME, A, B, C),
        ?CLAUSE(NAME, small, A, B, C);
        ?CLAUSE(NAME, medium, A, B, C);
        ?CLAUSE(NAME, large, A, B, C)).

-define(GEN_STUB(FUN), gen_stub_fun() -> FUN).
