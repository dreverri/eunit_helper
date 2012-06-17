-module(espec_tests).

-include_lib("eunit/include/eunit.hrl").
-include("espec.hrl").

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

-espec focus.
should_focus_when_filtered() ->
    ok.

-espec ignore.
should_be_ignored_when_filtered() ->
    ok.

%should_fail() ->
%    ?assert(false).

%-espec {timeout, 1}.
%should_timeout() -> 
%    timer:sleep(2000).
