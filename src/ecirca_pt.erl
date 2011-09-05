%%% @doc This module contains function to glue atoms in compile time.
%%%      The idea of replace_concat belongs to Zed from this SO question:
%%%      http://stackoverflow.com/questions/1423054/
%%%      erlang-is-there-an-equivalent-to-the-c-preprocessor-directive

-module(ecirca_pt).
-export([parse_transform/2]).
-include("ecirca.hrl").

parse_transform(AST, _Options) ->
    WithStubsDeep = [findfun(T, fun replace_stubfun/1) || T <- AST],
    WithStubs = lists:flatten(WithStubsDeep),
    [findfun(T, fun (Tree) ->
                        erl_syntax_lib:map(fun replace_concat/1, Tree)
                end) || T <- WithStubs].

findfun({function, _, _, _, _} = Tree, OnFun) -> OnFun(Tree);
findfun(Tree, _) -> Tree.

replace_concat(Tree) ->
    Stx = try
              infix_expr = erl_syntax:type(Tree),
              Op = erl_syntax:infix_expr_operator(Tree),
              Left = erl_syntax:infix_expr_left(Tree),
              Right = erl_syntax:infix_expr_right(Tree),
              '++' = erl_syntax:operator_name(Op),
              atom = erl_syntax:type(Left),
              atom = erl_syntax:type(Right),
              erl_syntax:atom(erl_syntax:atom_literal(Left)
                              ++ "_"
                              ++  erl_syntax:atom_literal(Right))
          catch
              error:{badmatch, _} -> Tree
          end,
    erl_syntax:revert(Stx).

replace_stubfun(Tree) ->
    try
        %% filtering only functions of the form gen_stub_fun() -> ...
        StubNameTree = erl_syntax:function_name(Tree),
        gen_stub_fun = erl_syntax:atom_value(StubNameTree),
        [Clause] = erl_syntax:function_clauses(Tree),
        [] = erl_syntax:clause_patterns(Clause),
        [ClauseBody] = erl_syntax:clause_body(Clause),
        %% extracting neccesary information
        Fun = erl_syntax:implicit_fun_name(ClauseBody),
        NameTree = erl_syntax:arity_qualifier_body(Fun),
        ArityTree = erl_syntax:arity_qualifier_argument(Fun),
        Name = erl_syntax:atom_value(NameTree),
        Arity = erl_syntax:integer_value(ArityTree),
        Line = erl_syntax:get_pos(ClauseBody),
        catch [erl_syntax:revert(build_stubfun(Name, Type, Arity, Line))
         || Type <- [small, medium, large]]
    catch
        error:{badmatch, _} -> erl_syntax:revert(Tree)
    end.

build_stubfun(Name, Type, Arity, Line) when is_atom(Name),
                                            is_atom(Type)->
    FullName = atom_to_list(Name) ++ "_" ++ atom_to_list(Type),
    NameTree = erl_syntax:atom(FullName),
    Pattern = [erl_syntax:underscore() || _ <- lists:seq(1, Arity)],
    NotLoadedCall = erl_syntax:application(erl_syntax:atom(not_loaded),
                                           [erl_syntax:integer(Line)]),
    Clause = erl_syntax:clause(Pattern, none, [NotLoadedCall]),
    erl_syntax:function(NameTree, [Clause]).
