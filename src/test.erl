-module(test).
-compile({parse_transform, pt}).
-export([foo/1]).

foo(X) -> X.
