%% -*- coding: utf-8 -*-
-module(addlog_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

suite() ->
    [{timetrap,{minutes,1}}].

all() -> [
    test1,
    post_version_of_addlog_method,
    cyrillic_addlog_test,
    strange_imei_addlog_test,
    hwid, hwid2,
    balance
].

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

post_version_of_addlog_method(Config) ->
    % Text = list_to_binary(helper:url_encode(helper:random_string())),
    Text = <<"Что, суки, не ждали?"/utf8>>,
    TextEncoded = list_to_binary(helper:url_encode(Text)),
    {200, _, <<"ADDLOG: OK\r\n">>} = helper:post(Config, "/addlog", #{}, TextEncoded),

    Skey = base64:encode(?config(imei, Config)),
    [Doc] = navidb:get_logs(Skey, 20, 100000000000),

    ?assertMatch(#{<<"system">> := Skey, <<"text">> := Text}, Doc),
    ok.

cyrillic_addlog_test(Config) ->
    Text = <<"Текст на русском и не только 🤘🐱🌧☠."/utf8>>,

    {200, _, <<"ADDLOG: OK\r\n">>} = helper:get(Config, "/addlog", #{text => Text}),

    Skey = base64:encode(?config(imei, Config)),
    [Doc] = navidb:get_logs(Skey, 20, 100000000000),
    ?assertMatch(#{<<"system">> := Skey, <<"text">> := Text}, Doc),
    ok.

strange_imei_addlog_test(ConfigBase) ->
    Text = <<"Тест для нестандартного IMEI."/utf8>>,

    Imei = <<"123+123">>,
    Config = [{imei, Imei} | ConfigBase],

    {200, _, <<"ADDLOG: OK\r\n">>} = helper:get(Config, "/addlog", #{text => Text}),

    Skey = base64:encode(?config(imei, Config)),
    [Doc|_] = navidb:get_logs(Skey, 20, 100000000000),
    ?assertMatch(#{<<"system">> := Skey, <<"text">> := Text}, Doc),
    ok.


hwid(Config) ->
    Text = "Test message. Version line HWID:<b>3081</b> SWID:<b>302E</b>",
    {200, _, <<"ADDLOG: OK\r\n">>} = helper:get(Config, "/addlog", #{text => Text}),

    Skey = base64:encode(?config(imei, Config)),
    % [Doc] = navidb:get_logs(Skey, 20, 100000000000),
    % ?assertMatch(#{system := Skey, text := Text}, Doc),
    System = navidb:get(systems, {id, Skey}),
    ?assertMatch(#{<<"hwid">> := <<"3081">>, <<"swid">> := <<"302E">>}, System),
    ok.

hwid2(Config) ->
    Text = <<"Трекер 6001-05 включён. HWID:<b>6001-05</b> SWID:<b>1.2.2-wip_debug</b>. Режим: <b>Beacon</b>"/utf8>>,
    {200, _, <<"ADDLOG: OK\r\n">>} = helper:get(Config, "/addlog", #{text => Text}),

    Skey = base64:encode(?config(imei, Config)),
    % [Doc] = navidb:get_logs(Skey, 20, 100000000000),
    % ?assertMatch(#{system := Skey, text := Text}, Doc),
    System = navidb:get(systems, {id, Skey}),
    ?assertMatch(#{<<"hwid">> := <<"6001-05">>, <<"swid">> := <<"1.2.2-wip_debug">>}, System),
    ok.


balance(Config) ->
    Payload = #{
        mtype => "balance",
        value => 20,
        text  => "Balance is 20"
    },
    {200, _, <<"ADDLOG: OK\r\n">>} = helper:get(Config, "/addlog", Payload),

    Skey = base64:encode(?config(imei, Config)),
    % [Doc] = navidb:get_logs(Skey, 20, 100000000000),
    % ?assertMatch(#{system := Skey, text := Text}, Doc),
    System = navidb:get(systems, {id, Skey}),
    ?assertMatch(#{<<"balance">> := #{<<"value">> := 20}}, System),
    ok.
