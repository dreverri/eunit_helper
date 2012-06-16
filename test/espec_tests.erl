-module(espec_tests).

-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform, espec_parse_transform}).

-export([setup/0,
        cleanup/1,
        setup_each/1,
        cleanup_each/2]).

-export([should_do_stuff/0,
        should_do_other_stuff/0]).

espec_test_() ->
    espec:init(?MODULE).

setup() ->
    ?debugMsg("setup").

cleanup(_) ->
    ?debugMsg("cleanup").

setup_each(_) ->
    ?debugMsg("setup_each").

cleanup_each(_, _) ->
    ?debugMsg("cleanup_each").

-espec {timeout, 100}.
should_do_stuff() -> ok.

-espec {timeout, 10}.
-espec {foo, bar}.
-espec [{a, b}, {d, e}].
should_do_other_stuff() -> ok.
