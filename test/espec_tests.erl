-module(espec_tests).

-compile({parse_transform, espec_parse_transform}).

-export([setup/0,
        cleanup/1,
        setup_each/1,
        cleanup_each/2]).

-include_lib("eunit/include/eunit.hrl").

espec_test_() ->
    espec:init(?MODULE).

setup() ->
    foo.

cleanup(foo) ->
    ok.

setup_each(foo) ->
    bar.

cleanup_each(foo, bar) ->
    ok.

should_receive_bar(bar) ->
    ok.

should_receive_foo_and_bar(foo, bar) ->
    ok.

%-espec {timeout, 1}.
%should_timeout() -> 
%    timer:sleep(2000).
