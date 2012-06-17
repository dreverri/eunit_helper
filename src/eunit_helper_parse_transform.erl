-module(eunit_helper_parse_transform).

-export([parse_transform/2]).

-record(state, {acc=[], opts=[], funs=[], exports=[], fixtures=[], module, 
        init=true}).

-define(FIXTURES, [{setup,0},{cleanup,1},{setup_each,1},{cleanup_each,2}]).

parse_transform(Forms, _) ->
    walk_ast(Forms).

walk_ast(Forms) ->
    %io:format("Forms: ~p~n", [Forms]),
    walk_ast(Forms, #state{}).

walk_ast([], State) ->
    [File, Module|Rest] = lists:reverse(State#state.acc),
    NewForms = [File, Module, eunit_helper(State), export(State)|Rest],
    case State#state.init of
        true ->
            NewForms ++ [eunit_helper_init(State)];
        false ->
            NewForms
    end;

walk_ast([{attribute, _Line, eunit_helper, Value}|Rest], State) ->
    Opts = merge_opts(Value, State#state.opts),
    walk_ast(Rest, State#state{opts=Opts});

walk_ast([Attr={function, _, eunit_helper_test_, 0, _}|Rest], State) ->
    Acc = [Attr|State#state.acc],
    walk_ast(Rest, State#state{acc=Acc, opts=[], init=false});

walk_ast([Attr={function, _, Name, Arity, _}|Rest], State) ->
    State1 = case lists:member({Name, Arity}, ?FIXTURES) of
        true ->
            Fixtures = [{Name,Arity}|State#state.fixtures],
            State#state{fixtures=Fixtures};
        false ->
            case lists:prefix("should_", atom_to_list(Name)) of
                true ->
                    Funs = [{{Name, Arity}, State#state.opts}|State#state.funs],
                    State#state{funs=Funs};
                false ->
                    State
            end
    end,
    Acc = [Attr|State1#state.acc],
    walk_ast(Rest, State1#state{acc=Acc, opts=[]});

walk_ast([Attr={attribute, _, export, Exports}|Rest], State) ->
    Exports1 = Exports ++ State#state.exports,
    Acc = [Attr|State#state.acc],
    walk_ast(Rest, State#state{exports=Exports1, acc=Acc});

walk_ast([Attr={attribute, _, module, Module}|Rest], State) ->
    Acc = [Attr|State#state.acc],
    walk_ast(Rest, State#state{module=Module, acc=Acc});

walk_ast([Attr|Rest], State) ->
    Acc = [Attr|State#state.acc],
    walk_ast(Rest, State#state{acc=Acc}).

merge_opts(Value, Espec) when is_list(Value) ->
    [expand_value(V) || V <- Value] ++ Espec;

merge_opts(Value, Espec) ->
    [expand_value(Value)|Espec].

expand_value(V) when is_tuple(V) andalso size(V) =:= 2 ->
    V;

expand_value(V) ->
    {V, true}.

eunit_helper(State) ->
    {attribute, 0, eunit_helper, State#state.funs}.

eunit_helper_init(State) ->
    {function,0,eunit_helper_test_,0,
        [{clause,0,[],[],
                [{call,0,
                        {remote,0,{atom,0,eunit_helper},{atom,0,init}},
                        [{atom,0,State#state.module}]}]}]}.

export(State) ->
    Funs = [Fun || {Fun, _} <- State#state.funs] ++ State#state.fixtures ++ [{eunit_helper_test_,0}],
    Exports = [F || F <- Funs, not lists:member(F, State#state.exports)],
    {attribute, 0, export, Exports}.
