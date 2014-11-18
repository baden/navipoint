-module(addlog_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

suite() ->
    [{timetrap,{minutes,1}}].

all() -> [ test1 ].

% -define(POINT_PORT, 8981).

init_per_suite(Config) ->
    ct:pal("---------------- init_per_suite(~p)", [Config]),
    error_logger:tty(false),

    {ok, Modules} = application:ensure_all_started(navipoint),
    {ok, GunModules} = application:ensure_all_started(gun),

    [{modules, Modules ++ GunModules} | Config].

end_per_suite(Config) ->
    ct:pal("end_per_suite(~p)", [Config]),
    Modules = ?config(modules, Config),
    [application:stop(Module) || Module <- lists:reverse(Modules)],
    % application:unload(lager), application:unload(navidb), application:unload(naviccapi),
    application:unload(navipoint),
    meck:unload(),
    error_logger:tty(true),
    ok.

init_per_testcase(Case, Config) ->
    ct:pal("init_per_testcase(~p, ~p)", [Case, Config]),
    Config.

end_per_testcase(Case, Config) ->
    ct:pal("end_per_testcase(~p, ~p)", [Case, Config]),
    Config.

test1(Config) ->
    ct:pal("test1(~p)", [Config]),
    % {ok, ConnPid} = gun:open("localhost", 8982, [{retry, 0}, {type, tcp}]),

    % Out of REST API
    % {200, _, _} = apiget("/1.0/info"),
    ok.

random_string() ->
    base64:encode(crypto:rand_bytes(16)).
