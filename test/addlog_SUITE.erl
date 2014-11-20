-module(addlog_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

suite() ->
    [{timetrap,{minutes,1}}].

all() -> [ test1, hwid, balance ].

% -define(POINT_PORT, 8981).

init_per_suite(Config) ->
    error_logger:tty(false),

    {ok, Modules} = application:ensure_all_started(navipoint),
    {ok, GunModules} = application:ensure_all_started(gun),

    [{modules, Modules ++ GunModules} | Config].

end_per_suite(Config) ->
    Modules = ?config(modules, Config),
    [application:stop(Module) || Module <- lists:reverse(Modules)],
    % application:unload(lager), application:unload(navidb), application:unload(naviccapi),
    application:unload(navipoint),
    error_logger:tty(true),
    ok.

init_per_testcase(_Case, Config) ->
    Imei = helper:random_string(),
    [{imei, Imei} | Config].

end_per_testcase(_Case, Config) ->
    Config.

test1(Config) ->
    Imei = ?config(imei, Config),

    Text = helper:random_string(),
    {200, _, <<"ADDLOG: OK\r\n">>} = helper:get(Imei, "/addlog", #{text => Text}),

    Skey = base64:encode(Imei),
    [Doc] = navidb:get_logs(Skey, 20, 100000000000),
    ?assertMatch(#{system := Skey, text := Text}, Doc),
    ok.

hwid(Config) ->
    Imei = ?config(imei, Config),
    Text = "Test message. Version line HWID:<b>3081</b> SWID:<b>302E</b>",
    {200, _, <<"ADDLOG: OK\r\n">>} = helper:get(Imei, "/addlog", #{text => Text}),

    Skey = base64:encode(Imei),
    % [Doc] = navidb:get_logs(Skey, 20, 100000000000),
    % ?assertMatch(#{system := Skey, text := Text}, Doc),
    System = navidb:get(systems, {id, Skey}),
    ?assertMatch(#{hwid := <<"3081">>, swid := <<"302E">>}, System),
    ok.

balance(Config) ->
    Imei = ?config(imei, Config),
    Payload = #{
        mtype => "balance",
        value => 20,
        text  => "Balance is 20"
    },
    {200, _, <<"ADDLOG: OK\r\n">>} = helper:get(Imei, "/addlog", Payload),

    Skey = base64:encode(Imei),
    % [Doc] = navidb:get_logs(Skey, 20, 100000000000),
    % ?assertMatch(#{system := Skey, text := Text}, Doc),
    System = navidb:get(systems, {id, Skey}),
    ?assertMatch(#{balance := #{value := 20}}, System),
    ok.
