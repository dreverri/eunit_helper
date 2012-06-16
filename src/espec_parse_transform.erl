-module(espec_parse_transform).

-export([parse_transform/2]).

-record(state, {acc=[], opts=[], funs=[], exports=[]}).

parse_transform(Forms, _) ->
    walk_ast(Forms).

walk_ast(Forms) ->
    %%io:format("Forms: ~p~n", [Forms]),
    walk_ast(Forms, #state{}).

walk_ast([], State) ->
    [File, Module|Rest] = lists:reverse(State#state.acc),
    [File, Module, espec(State), export(State)|Rest];

walk_ast([{attribute, _Line, espec, Value}|Rest], State) ->
    walk_ast(Rest, State#state{opts=merge_opts(Value, State#state.opts)});

walk_ast([Fun={function, _, Name, Arity, _}|Rest], State) ->
    case lists:prefix("should_", atom_to_list(Name)) of
        true ->
            Funs = [{{Name, Arity}, State#state.opts}|State#state.funs],
            walk_ast(Rest, State#state{acc=[Fun|State#state.acc], opts=[], 
                    funs=Funs});
        false ->
            walk_ast(Rest, State#state{acc=[Fun|State#state.acc], opts=[]})
    end;

walk_ast([Export={attribute, _, export, Exports}|Rest], State) ->
    walk_ast(Rest, State#state{exports=Exports++State#state.exports, 
            acc=[Export|State#state.acc]});

walk_ast([Other|Rest], State) ->
    walk_ast(Rest, State#state{acc=[Other|State#state.acc]}).

merge_opts(Value, Espec) when is_list(Value) ->
    Value ++ Espec;

merge_opts(Value, Espec) ->
    [Value|Espec].

espec(State) ->
    {attribute, 0, espec, State#state.funs}.

export(State) ->
    {attribute, 0, export, [FA || {FA, _} <- State#state.funs,
            not lists:member(FA, State#state.exports)]}.
