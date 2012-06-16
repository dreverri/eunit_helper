-module(espec).

-export([init/1]).

-export([setup/1, cleanup/2, setup_each/2, cleanup_each/3, tests/2]).

-include_lib("eunit/include/eunit.hrl").

init(Module) ->
    {setup,
        fun() -> espec:setup(Module) end,
        fun(X) -> espec:cleanup(Module, X) end,
        fun(X1) ->
                {foreachx,
                    fun(X2) -> espec:setup_each(Module, X2) end,
                    fun(X3, R) -> espec:cleanup_each(Module, X3, R) end,
                    espec:tests(Module, X1)
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

tests(Module, X) ->
    Attributes = Module:module_info(attributes),
    Specs = proplists:get_value(espec, Attributes),
    convert_specs(Specs, Module, X, []).

convert_specs([Spec|Rest], M, X, Acc) ->
    Test = convert_spec(Spec, M, X),
    convert_specs(Rest, M, X, [Test|Acc]);

convert_specs([], _, _, Acc) ->
    Acc.

convert_spec({{F,A}, Opts}, M, X) ->
    {X, generate_test(M, F, A, Opts)}.

generate_test(M, F, A, Opts) ->
    wrap(M, F, A, Opts).

wrap(M, F, 0, Opts) -> fun(_, _) -> wrap1(M, F, [], Opts) end;
wrap(M, F, 1, Opts) -> fun(_, R) -> wrap1(M, F, [R], Opts) end;
wrap(M, F, 2, Opts) -> fun(X, R) -> wrap1(M, F, [X, R], Opts) end.

wrap1(M, F, A, Opts) ->
    Test = fun() -> apply(M, F, A) end,
    case proplists:get_value(timeout, Opts) of
        T when is_number(T) ->
            {timeout, T, Test};
        _ ->
            Test
    end.

catch_undef(M, F, A, Default) ->
    try apply(M, F, A) catch error:undef -> Default end.
