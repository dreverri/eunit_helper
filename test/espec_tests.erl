-module(espec_tests).

-export([setup/0,
        cleanup/1,
        setup_each/1,
        cleanup_each/2]).

-include_lib("eunit/include/eunit.hrl").
-include("espec.hrl").

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

%should_fail() ->
%    ?assert(false).

%-espec {timeout, 1}.
%should_timeout() -> 
%    timer:sleep(2000).
