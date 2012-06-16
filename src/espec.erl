-module(espec).

-export([init/1]).

-export([setup/1, cleanup/2, setup_each/2, cleanup_each/3, tests/2]).

-record(state, {module, context, tests=[]}).

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
    convert_specs(Specs, #state{module=Module, context=X}).

convert_specs([Spec|Rest], State) ->
    State1 = convert_spec(Spec, State),
    convert_specs(Rest, State1);

convert_specs([], State) ->
    State#state.tests.

convert_spec({{F,A}, Opts}, State) ->
    case matches_filters(Opts) of
        false ->
            State;
        true ->
            M = State#state.module,
            Test = {State#state.context, generate_test(M, F, A, Opts)},
            State#state{tests=[Test|State#state.tests]}
    end.

matches_filters(Opts) ->
    {Match, Ignore} = filters(),
    match_all(Match, Opts) andalso not match_any(Ignore, Opts).

match_all(Match, Opts) ->
    lists:all(fun(M) -> is_tagged(M,Opts) end, Match).

match_any(Ignore, Opts) ->
    lists:any(fun(I) -> is_tagged(I,Opts) end, Ignore).

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

catch_undef(M, F, A, Default) ->
    try apply(M, F, A) catch error:undef -> Default end.

filters() ->
    case application:get_env(espec, filters) of
        undefined ->
            Filters = get_filters(),
            ok = application:set_env(espec, filters, Filters),
            Filters;
        {ok, V} ->
            V
    end.

get_filters() ->
    case os:getenv("filter") of
        false ->
            {[], []};
        FilterString ->
            parse_filter_string(FilterString)
    end.

parse_filter_string(FilterString) ->
    Filters = string:tokens(FilterString, " "),
    lists:foldl(fun(F,{Match, Ignore}) ->
                case lists:prefix("-", F) of
                    true ->
                        {Match, [lists:nthtail(1,F)|Ignore]};
                    false ->
                        {[F|Match], Ignore}
                end
        end, {[],[]}, Filters).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(OPTS, [{"focus",true},{"ignore",true}]).

parse_empty_filter_string_test() ->
    {[],[]} = parse_filter_string("").

parse_matched_filter_string_test() ->
    {["focus"], []} = parse_filter_string("focus").

parse_ignore_filter_string_test() ->
    {[], ["focus"]} = parse_filter_string("-focus").

matches_filter_test() ->
    application:set_env(espec, filters, {["focus"], []}),
    ?assert(matches_filters(?OPTS)).

does_not_match_filter_test() ->
    application:set_env(espec, filters, {["focus"], []}),
    ?assertNot(matches_filters([])).

ignore_filter_test() ->
    application:set_env(espec, filters, {[], ["ignore"]}),
    ?assertNot(matches_filters(?OPTS)).

do_not_ignore_filter_test() ->
    application:set_env(espec, filters, {[], ["ignore"]}),
    ?assert(matches_filters([])).

-endif.
