-module(eunit_helper_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_helper.hrl").

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

-eunit_helper focus.
should_focus_when_filtered() ->
    ok.

-eunit_helper ignore.
should_be_ignored_when_filtered() ->
    ok.

%should_fail() ->
%    ?assert(false).

%-eunit_helper {timeout, 1}.
%should_timeout() -> 
%    timer:sleep(2000).
