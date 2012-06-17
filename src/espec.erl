-module(espec).

-export([init/1]).

-record(state, {module, context, tests=[]}).
-record(filter, {any=[], all=[], none=[]}).

-define(TRY(M,F,A,D), try apply(M, F, A) catch error:undef -> D end).

init(Module) ->
    {setup,
        fun() -> setup(Module) end,
        fun(X) -> cleanup(Module, X) end,
        fun(X1) ->
                {foreachx,
                    fun(X2) -> setup_each(Module, X2) end,
                    fun(X3, R) -> cleanup_each(Module, X3, R) end,
                    tests(Module, X1)
                }
        end}.

setup(Module) ->
    ?TRY(Module, setup, [], ok).

cleanup(Module, X) ->
    ?TRY(Module, cleanup, [X], X).

setup_each(Module, X) ->
    ?TRY(Module, setup_each, [X], X).

cleanup_each(Module, X, R) ->
    ?TRY(Module, cleanup_each, [X, R], R).

tests(Module, X) ->
    Attributes = Module:module_info(attributes),
    Specs = proplists:get_value(espec, Attributes),
    convert_specs(Specs, #state{module=Module, context=X}).

convert_specs([Spec|Rest], State) ->
    State1 = convert_spec(Spec, State),
    convert_specs(Rest, State1);

convert_specs([], State) ->
    State#state.tests.

convert_spec({{F,A}, Opts}, State) ->
    case should_run(Opts) of
        false ->
            State;
        true ->
            M = State#state.module,
            Test = {State#state.context, generate_test(M, F, A, Opts)},
            State#state{tests=[Test|State#state.tests]}
    end.

should_run(Opts) ->
    should_run(Opts, filter()).

should_run(Opts, Filter) ->
    match_all(Filter#filter.all, Opts) andalso
    match_any(Filter#filter.all ++ Filter#filter.any, Opts, true) andalso
    not match_any(Filter#filter.none, Opts).

match_all(List, Opts) ->
    match_all(List, Opts, true).

match_all([], _, Empty) ->
    Empty;

match_all(List, Opts, _) ->
    lists:all(fun(E) -> is_tagged(E,Opts) end, List).

match_any(List, Opts) ->
    match_any(List, Opts, false).

match_any([], _, Empty) ->
    Empty;

match_any(List, Opts, _) ->
    lists:any(fun(E) -> is_tagged(E,Opts) end, List).

is_tagged(Filter, Opts) ->
    proplists:get_value(Filter, Opts) =/= undefined.

generate_test(M, F, A, Opts) ->
    wrap(M, F, A, Opts).

wrap(M, F, 0, Opts) -> fun(_, _) -> t(M, F, [], Opts) end;
wrap(M, F, 1, Opts) -> fun(_, R) -> t(M, F, [R], Opts) end;
wrap(M, F, 2, Opts) -> fun(X, R) -> t(M, F, [X, R], Opts) end.

t(M, F, A, Opts) ->
    Title = iolist_to_binary(io_lib:format("~s:~s/~B", [M,F,length(A)])),
    Test = {Title, fun() -> apply(M, F, A) end},
    case proplists:get_value(timeout, Opts) of
        T when is_number(T) ->
            {timeout, T, Test};
        _ ->
            Test
    end.

filter() ->
    case os:getenv("filter") of
        false ->
            #filter{};
        FilterString ->
            parse_filter_string(FilterString)
    end.

parse_filter_string(FilterString) ->
    Tokens = string:tokens(FilterString, " "),
    lists:foldl(fun(T, Filter) ->
                case T of
                    "-" ++ T1 ->
                        None = [list_to_atom(T1)|Filter#filter.none],
                        Filter#filter{none=None};
                    "+" ++ T1 ->
                        All = [list_to_atom(T1)|Filter#filter.all],
                        Filter#filter{all=All};
                    _ ->
                        Any = [list_to_atom(T)|Filter#filter.any],
                        Filter#filter{any=Any}
                end
        end, #filter{}, Tokens).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(FOCUS_OPTS, [{focus,true}]).
-define(SLOW_OPTS, [{slow,true}]).
-define(FOCUS_SLOW_OPTS, [{focus,true},{slow,true}]).
-define(FOCUS, #filter{all=[focus]}).
-define(FOCUS_ANY, #filter{any=[focus]}).
-define(NOT_SLOW, #filter{none=[slow]}).
-define(FOCUS_OR_SLOW, #filter{any=[focus,slow]}).
-define(FOCUS_AND_SLOW, #filter{all=[focus,slow]}).

parse_filter_string_test() ->
    ?assertEqual(#filter{}, parse_filter_string("")),
    ?assertEqual(?FOCUS, parse_filter_string("+focus")),
    ?assertEqual(?FOCUS_ANY, parse_filter_string("focus")),
    ?assertEqual(?NOT_SLOW, parse_filter_string("-slow")),
    ?assertEqual(?FOCUS_OR_SLOW, parse_filter_string("slow focus")),
    ?assertEqual(?FOCUS_AND_SLOW, parse_filter_string("+slow +focus")).

should_run_test() ->
    ?assert(should_run(?FOCUS_OPTS, ?FOCUS)),
    ?assert(should_run(?FOCUS_OPTS, ?NOT_SLOW)),
    ?assert(should_run(?FOCUS_OPTS, ?FOCUS_OR_SLOW)),
    ?assertNot(should_run(?FOCUS_OPTS, ?FOCUS_AND_SLOW)),
    ?assertNot(should_run(?SLOW_OPTS, ?FOCUS)),
    ?assertNot(should_run(?SLOW_OPTS, ?NOT_SLOW)),
    ?assert(should_run(?SLOW_OPTS, ?FOCUS_OR_SLOW)),
    ?assertNot(should_run(?SLOW_OPTS, ?FOCUS_AND_SLOW)),
    ?assert(should_run(?FOCUS_SLOW_OPTS, ?FOCUS)),
    ?assertNot(should_run(?FOCUS_SLOW_OPTS, ?NOT_SLOW)),
    ?assert(should_run(?FOCUS_SLOW_OPTS, ?FOCUS_OR_SLOW)),
    ?assert(should_run(?FOCUS_SLOW_OPTS, ?FOCUS_AND_SLOW)).

-endif.
