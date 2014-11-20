-module(params_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

suite() ->
    [{timetrap,{minutes,1}}].

all() -> [ empty, data ].

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

empty(Config) ->
    Imei = ?config(imei, Config),
    {200, _, <<"FINISH\r\n">>} = helper:get(Imei, "/params", #{cmd => <<"params">>}),
    ok.

data(Config) ->
    Imei = ?config(imei, Config),
    Skey = base64:encode(Imei),

    Body = lists:flatten([
        "service.lock INT 2222 3333\n",
        "protect.me INT 777 777\n",
        "secure.code STR32 \"2-2-2-2\" \"0-0-0-0\"\n",
        "END\n"
    ]),
    {200, _, <<"CONFIG: OK\r\n">>} = helper:post(Imei, "/config", #{cmd => <<"save">>}, Body),
    ?assertMatch(
        #{data := #{
            'service.lock' := #{type := <<"INT">>,   value := <<"2222">>,    default := <<"3333">>},
            'protect.me'   := #{type := <<"INT">>,   value := <<"777">>,     default := <<"777">>},
            'secure.code'  := #{type := <<"STR32">>, value := <<"2-2-2-2">>, default := <<"0-0-0-0">>}
        }},
        navidb:get(params, {id, Skey})
    ),

    Queue = #{
        'service.lock' => <<"1111">>,
        'secure.code'  => <<"1-1-1-1">>
    },

    navidb:set(params, Skey, #{queue => Queue}),
    % Может это правильнее перенести в navidb?
    Command = <<"CONFIGUP\r\n">>,
    navidb:set(command, Skey, Command),

    Text = helper:random_string(),
    {200, _, <<"CONFIGUP\r\nADDLOG: OK\r\n">>} = helper:get(Imei, "/addlog", #{text => Text}),
    % ct:pal("Response1 = ~p", [Response1]),

    {200, _, Response} = helper:get(Imei, "/params", #{cmd => <<"params">>}),

    Success = case Response of
        <<"PARAM service.lock 1111\r\nPARAM secure.code 1-1-1-1\r\nFINISH\r\n">> ->
            true;
        <<"PARAM PARAM secure.code 1-1-1-1\r\nservice.lock 1111\r\nFINISH\r\n">> ->
            true;
        _ ->
            false
    end,
    ?assertEqual(true, Success),

    {200, _, <<"CONFIRM\r\n">>} = helper:get(Imei, "/params", #{cmd => <<"confirm">>}),

    ?assertMatch(
        #{data := #{
            'service.lock' := #{type := <<"INT">>,   value := <<"1111">>,    default := <<"3333">>},
            'protect.me'   := #{type := <<"INT">>,   value := <<"777">>,     default := <<"777">>},
            'secure.code'  := #{type := <<"STR32">>, value := <<"1-1-1-1">>, default := <<"0-0-0-0">>}
        }},
        navidb:get(params, {id, Skey})
    ),



    % Text = helper:random_string(),
    % {200, _, <<"CONFIGUP\r\nADDLOG: OK\r\n">>} = helper:get(Imei, "/addlog", #{text => Text}),
    % % ct:pal("Response1 = ~p", [Response1]),


    ok.
