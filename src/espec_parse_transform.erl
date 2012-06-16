-module(espec_parse_transform).

-export([parse_transform/2]).

parse_transform(Forms, _) ->
    walk_ast(Forms).

walk_ast(Forms) ->
    walk_ast(Forms, [], [], []).

walk_ast([], Acc, _RelEspec, AbsEspec) ->
    [File, Module|Rest] = lists:reverse(Acc),
    [File, Module, {attribute, 0, espec, AbsEspec}|Rest];

walk_ast([{attribute, _Line, espec, Value}|Rest], Acc, RelEspec, AbsEspec) ->
    walk_ast(Rest, Acc, merge_espec(Value, RelEspec), AbsEspec);

walk_ast([Fun={function, _, Name, Arity, _}|Rest], Acc, RelEspec, AbsEspec) ->
    walk_ast(Rest, [Fun|Acc], [], [{{Name, Arity}, RelEspec}|AbsEspec]);

walk_ast([Other|Rest], Acc, RelEspec, AbsEspec) ->
    walk_ast(Rest, [Other|Acc], RelEspec, AbsEspec).

merge_espec(Value, Espec) when is_list(Value) ->
    Value ++ Espec;

merge_espec(Value, Espec) ->
    [Value|Espec].
