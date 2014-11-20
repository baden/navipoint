-module(bingps_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

suite() ->
    [{timetrap,{minutes,1}}].

all() -> [ errors, e2 ].

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


errors(Config) ->
    Imei = ?config(imei, Config),
    Skey = base64:encode(Imei),

    Body1 = <<16#FF, 16#E2>>,
    {200, _, <<"BINGPS: DATAERROR\r\n">>} = helper:post(Imei, "/bingps", #{}, Body1),
    #{dynamic := #{error := <<"dataerror">>}} = navidb:get(systems, Skey),

    Body2 = <<0:(32*8)/little-unsigned-integer, 1, 2>>,
    {200, _, <<"BINGPS: CRCERROR\r\n">>} = helper:post(Imei, "/bingps", #{}, Body2),
    #{dynamic := #{error := <<"crcerror">>}} = navidb:get(systems, Skey),

    ok.


e2(Config) ->
    Imei = ?config(imei, Config),

    Datetime = 1409680257, % 2014-09-02 17:50:57
    Latitude = 48501829, % 48.501829
    Longitude = 34623322,  % 34.623322

    io:format("Datetime = ~p~n", [Datetime]),

    FakeE2Packet = <<16#FF, 16#E2,
    0, 0,
    Datetime:32/little-unsigned-integer,
    Latitude:32/little-unsigned-integer,
    Longitude:32/little-unsigned-integer,
    0:(16*8)/little-unsigned-integer>>,

    ct:pal("FakeE2Packet = ~p~n", [FakeE2Packet]),
    CRC = helper:crc(FakeE2Packet),
    Body = <<FakeE2Packet/binary, CRC:16/little-unsigned-integer>>,
    Extra = #{
        csq  => 21,
        vout => 13970,
        vin  => 4170,
        dataid => <<"00015B86">>
    },
    {200, _, <<"BINGPS: OK\r\n">>} = helper:post(Imei, "/bingps", Extra, Body),

    Skey = base64:encode(Imei),
    {ok, FakeE2Packet} = navidb:get_geos(Skey, Datetime div 3600, (Datetime div 3600) + 1),

    #{dynamic := #{
        csq       := 21,
        dt        := 1409680257,
        latitude  := 48.501829,
        longitude := 34.623322,
        vin       := 4170,
        vout      := 13970
    }} = navidb:get(systems, Skey),

    ok.
