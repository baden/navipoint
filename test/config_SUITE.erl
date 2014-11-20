-module(config_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

suite() ->
    [{timetrap,{minutes,1}}].

all() -> [ save, phone ].

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

save(Config) ->
    Imei = ?config(imei, Config),
    Body = lists:flatten([
        "gps.T0.2    INT 10  10\n",
        "akkum.U.0   INT 862 862\n",
        "akkum.U.1   INT 907 907\n",
        "gsm.server  STR32 \"point.new.navi.cc\" \"map.navi.cc\"\n",
        "END\n"
    ]),

    {200, _, <<"CONFIG: OK\r\n">>} = helper:post(Imei, "/config", #{cmd => <<"save">>}, Body),

    Skey = base64:encode(Imei),
    #{data := Params} = navidb:get(params, {id, Skey}),
    ?assertMatch(#{
        'gps.T0.2'   := #{default := <<"10">>, type := <<"INT">>, value := <<"10">>},
        'akkum.U.0'  := #{default := <<"862">>,type := <<"INT">>, value := <<"862">>},
        'akkum.U.1'  := #{default := <<"907">>,type := <<"INT">>, value := <<"907">>},
        'gsm.server' := #{default := <<"map.navi.cc">>, type := <<"STR32">>, value := <<"point.new.navi.cc">>}
    }, Params),

    ok.

phone(Config) ->
    Imei = ?config(imei, Config),
    Body = lists:flatten([
        "gps.T0.2    INT 10  10\n",
        "END\n"
    ]),

    {200, _, <<"CONFIG: OK\r\n">>} = helper:post(Imei, "/config", #{cmd => <<"save">>, phone => <<"+380679332332">>}, Body),

    Skey = base64:encode(Imei),
    #{data := Params} = navidb:get(params, {id, Skey}),
    ?assertMatch(#{
        'gps.T0.2'   := #{default := <<"10">>, type := <<"INT">>, value := <<"10">>}
    }, Params),
    #{phone := <<"+380679332332">>} = navidb:get(systems, Skey),

    ok.
