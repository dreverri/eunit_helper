-module(espec).

-export([init/1]).

init(Module) ->
    {setup,
        fun() -> setup(Module) end,
        fun(X) -> cleanup(Module, X) end,
        fun(X1) ->
                {foreachx,
                    fun(X2) -> setup_each(Module, X2) end,
                    fun(X3, R) -> cleanup_each(Module, X3, R) end,
                    tests(Module, "should_", X1)
                }
        end}.

setup(Module) ->
    catch_undef(Module, setup, [], ok).

cleanup(Module, X) ->
    catch_undef(Module, cleanup, [X], X).

setup_each(Module, X) ->
    catch_undef(Module, setup_each, [X], X).

cleanup_each(Module, X, R) ->
    catch_undef(Module, cleanup_each, [X, R], R).

tests(Module, Prefix, X) ->
    % TODO: filter for arities 0,1,2
    Functions = [{Module, F, A, X} || {F, A} <- Module:module_info(functions),
        lists:prefix(Prefix, atom_to_list(F))],
    lists:foldl(fun convert/2, [], Functions).

convert({M, F, A, X}, Acc) -> [pair(X, M, F, A)|Acc].

pair(X, M, F, 0) -> {X, fun(_, _) -> wrap(M, F, []) end};
pair(X, M, F, 1) -> {X, fun(_, R) -> wrap(M, F, [R]) end};
pair(X, M, F, 2) -> {X, fun(X1, R) -> wrap(M, F, [X1, R]) end}.

wrap(M, F, A) -> fun() -> apply(M, F, A) end.

catch_undef(M, F, A, Default) ->
    try apply(M, F, A) catch error:undef -> Default end.
