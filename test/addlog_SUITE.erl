-module(addlog_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

suite() ->
    [{timetrap,{minutes,1}}].

all() -> [ test1, hwid, balance ].

% -define(POINT_PORT, 8981).

init_per_suite(Config) ->
    helper:start(Config).

end_per_suite(Config) ->
    helper:stop(Config).

init_per_testcase(_Case, Config) ->
    Imei = helper:random_string(),
    [{imei, Imei} | Config].

end_per_testcase(_Case, Config) ->
    Config.

test1(Config) ->
    Text = helper:random_string(),
    {200, _, <<"ADDLOG: OK\r\n">>} = helper:get(Config, "/addlog", #{text => Text}),

    Skey = base64:encode(?config(imei, Config)),
    [Doc] = navidb:get_logs(Skey, 20, 100000000000),
    ?assertMatch(#{<<"system">> := Skey, <<"text">> := Text}, Doc),
    ok.

hwid(Config) ->
    Text = "Test message. Version line HWID:<b>3081</b> SWID:<b>302E</b>",
    {200, _, <<"ADDLOG: OK\r\n">>} = helper:get(Config, "/addlog", #{text => Text}),

    Skey = base64:encode(?config(imei, Config)),
    % [Doc] = navidb:get_logs(Skey, 20, 100000000000),
    % ?assertMatch(#{system := Skey, text := Text}, Doc),
    System = navidb:get(systems, Skey),
    ?assertMatch(#{<<"hwid">> := <<"3081">>, <<"swid">> := <<"302E">>}, System),
    ok.

balance(Config) ->
    Payload = #{
        <<"mtype">> => "balance",
        <<"value">> => 20,
        <<"text">>  => "Balance is 20"
    },
    {200, _, <<"ADDLOG: OK\r\n">>} = helper:get(Config, "/addlog", Payload),

    Skey = base64:encode(?config(imei, Config)),
    % [Doc] = navidb:get_logs(Skey, 20, 100000000000),
    % ?assertMatch(#{system := Skey, text := Text}, Doc),
    System = navidb:get(systems, Skey),
    ?assertMatch(#{<<"balance">> := #{<<"value">> := 20}}, System),
    ok.
