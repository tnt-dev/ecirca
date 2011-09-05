-module(pt).
-export([parse_transform/2]).


parse_transform(Forms, Opts) ->
    parse_trans:top(fun transform/2, Forms, Opts).

%replace_logfun(function, Forms, Ctxt, Acc) ->
%    MFA = erl_syntax_lib:analyze_application(Forms),
%    {Name, N} = erl_syntax_lib:analyze_function(Forms),
%    _NewForms = parse_trans:do_insert_forms(below, [NewFun], Forms, Ctxt),
%    {Forms, false, Acc};
%replace_logfun(_, Form, _Ctxt, Acc) ->
%    {Form, false, Acc}.

transform(Forms, Ctxt) ->
    [Clauses] = parse_trans:do_inspect(fun inspect/4, [], Forms, Ctxt),
    F2 = erl_syntax:function(erl_syntax:atom(bar), Clauses),
    NewForms = parse_trans:do_insert_forms(below, [F2], Forms, Ctxt),
    io:format("F2: ~p~n", [F2]),
    io:format("~p~n", [NewForms]),
    parse_trans:revert(NewForms).


inspect(function, Forms, _, Acc) ->
%    {Name, N} = erl_syntax_lib:analyze_function(Forms),
%    {false, [{Name, N} | Acc]};
    Clauses = erl_syntax:function_clauses(Forms),
    {false, [Clauses | Acc]};
inspect(_, _, _, Acc) ->
    {false, Acc}.


