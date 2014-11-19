-module(config_SUITE).

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
    error_logger:tty(true),
    ok.

init_per_testcase(_Case, Config) ->
    Imei = helper:random_string(),
    [{imei, Imei} | Config].

end_per_testcase(_Case, Config) ->
    Config.

test1(Config) ->
    Imei = ?config(imei, Config),
    Body = lists:flatten([
        "gps.T0.2    INT 10  10\n",
        "gps.T0.3    INT 10  10\n",
        "akkum.U.1   INT 907 907\n",
        "akkum.U.0   INT 862 862\n",
        "akkum.U.3   INT 984 984\n",
        "akkum.U.2   INT 911 911\n",
        "akkum.U.4   INT 911 911\n",
        "gps.V0.2    INT 10  10\n",
        "gps.V0.3    INT 20  20\n",
        "gps.V0.0    INT 5   5\n",
        "gps.V0.1    INT 20  20\n",
        "gps.T4.0    INT 720 721\n",
        "gps.T4.1    INT 240 240\n",
        "gps.T4.2    INT 720 720\n",
        "gps.AOFF.0  INT 10 10\n",
        "gsm.server  STR32 \"point.new.navi.cc\" \"map.navi.cc\"\n",
        "END\n"
    ]),

    {200, _, <<"CONFIG: OK\r\n">>} = helper:post(Imei, "/config", #{cmd => <<"save">>, phone => <<"+380679332332">>}, Body),

    Skey = base64:encode(Imei),
    Params = navidb:get(params, {'_id', Skey}),
    ct:pal("Params = ~p", [Params]),
    ok.
