-module(bingps_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

suite() ->
    [{timetrap,{minutes,1}}].

all() -> [ errors, e1, e2, cclk ].

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

errors(Config) ->
    Skey = base64:encode(?config(imei, Config)),

    Body1 = <<16#FF, 16#E2>>,
    {200, _, <<"BINGPS: DATAERROR\r\n">>} = helper:post(Config, "/bingps", #{}, Body1),
    #{dynamic := #{error := <<"dataerror">>}} = navidb:get(systems, Skey),

    Body2 = <<0:(32*8)/little-unsigned-integer, 1, 2>>,
    {200, _, <<"BINGPS: CRCERROR\r\n">>} = helper:post(Config, "/bingps", #{}, Body2),
    #{dynamic := #{error := <<"crcerror">>}} = navidb:get(systems, Skey),

    ok.

e1(Config) ->
  MCC = 255,
  MNC = 03,

  Datetime = 1409680257, % 2014-09-02 17:50:57

  LAC0 = 16#B7E8,
  CID0 = 16#05B5, RXL0 = 52,
  CID1 = 16#05B6, RXL1 = 32,
  CID2 = 16#24F5, RXL2 = 30,
  CID3 = 16#43FB, RXL3 = 26,
  CID4 = 16#05B7, RXL4 = 24,
  CID5 = 16#7079, RXL5 = 19,
  CID6 = 16#43F9, RXL6 = 13,

  FakeE1Packet = <<16#FF, 16#E1,
  ((MCC-200)*100 + MNC):16/little-unsigned-integer, % | MCC+MNC   | 2 | MCC - код страны и MNC - код сети. (MCC-200)*100 + MNC |
  LAC0:16/little-unsigned-integer, % | LAC       | 2 | LAC - код локальной зоны (другими словами, совокупности базовых станций, обслуживаемых одним контроллером) |
  CID0:16/little-unsigned-integer, % | CID       | 2 | CID (CellID) - идентификатор, состоит из номеров базовой станции и сектора |
  Datetime:32/little-unsigned-integer,  % | DATETIME  | 4 | Дата+время (метка может быть не задана или иметь неточное значение)
  CID1:16/little-unsigned-integer, % | CID1      | 2 | CID - соседней вышки №1 |
  CID2:16/little-unsigned-integer, % | CID2      | 2 | CID - соседней вышки №2 |
  CID3:16/little-unsigned-integer, % | CID3      | 2 | CID - соседней вышки №3 |
  CID4:16/little-unsigned-integer, % | CID4      | 2 | CID - соседней вышки №4 |
  CID5:16/little-unsigned-integer, % | CID5      | 2 | CID - соседней вышки №5 |
  CID6:16/little-unsigned-integer, % | CID6      | 2 | CID - соседней вышки №6 |
  RXL0, % | RXL       | 1 | RXL - активной вышки |
  RXL1, % | RXL1      | 1 | RXL - соседней вышки №1 |
  RXL2, % | RXL2      | 1 | RXL - соседней вышки №2 |
  RXL3, % | RXL3      | 1 | RXL - соседней вышки №3 |
  RXL4, % | RXL4      | 1 | RXL - соседней вышки №4 |
  RXL5, % | RXL5      | 1 | RXL - соседней вышки №5 |
  RXL6, % | RXL6      | 1 | RXL - соседней вышки №6 |
  % | CRC       | 1 | ? |
  0:(1*8)/little-unsigned-integer>>,

  ?assertEqual(32, size(FakeE1Packet)),

  CRC = helper:crc(FakeE1Packet),
  Body = <<FakeE1Packet/binary, CRC:16/little-unsigned-integer>>,
  Extra = #{
    csq  => 21,
    vout => 13970,
    vin  => 4170,
    dataid => <<"00015B86">>
  },
  {200, _, <<"BINGPS: OK\r\n">>} = helper:post(Config, "/bingps", Extra, Body),

  Skey = base64:encode(?config(imei, Config)),
  {ok, _FakeE1Packet} = navidb:get_geos(Skey, Datetime div 3600, (Datetime div 3600) + 1),
  ct:pal("FakeE1Packet = ~p~n", [_FakeE1Packet]),

  ok.

e2(Config) ->
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
    {200, _, <<"BINGPS: OK\r\n">>} = helper:post(Config, "/bingps", Extra, Body),

    Skey = base64:encode(?config(imei, Config)),
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

cclk(Config) ->
    {200, _, Resp} = helper:get(Config, "/bingps", #{cmd => <<"CCLK">>}),
    ct:pal("Resp = ~p", [Resp]),
    {match, [Year, Month, Day, Hour, Minutes, Seconds]} = re:run(
        Resp,
        "^CCLK: (\\d\\d)\\/(\\d\\d)\\/(\\d\\d),(\\d\\d):(\\d\\d):(\\d\\d)\\+00$",
        [multiline, {newline, anycrlf}, {capture, all_but_first, list}]
    ),
    ct:pal("Year = ~p, Month = ~p, Day = ~p, Hour = ~p, Minutes = ~p, Seconds = ~p", [Year, Month, Day, Hour, Minutes, Seconds]),
    {match, [Dt]} = re:run(
        Resp,
        "^DT: ([0-9A-Za-z]{8})$",
        [multiline, {newline, anycrlf}, {capture, all_but_first, list}]
    ),
    ct:pal("Dt = ~p", [Dt]),
    {match, [CCLK]} = re:run(
        Resp,
        "^CCLK: (.+)$",
        [multiline, {newline, anycrlf}, {capture, all_but_first, list}]
    ),
    ct:pal("CCLK = ~p", [CCLK]),
    Dt1 = erlang:list_to_integer(Dt, 16),
    Expect = ec_date:format("y/m/d,H:i:s\\+\\0\\0", {Dt1 div 1000000, Dt1 rem 1000000, 0}),
    ct:pal("Expect = ~p", [Expect]),
    ?assertEqual(CCLK, Expect),
    ok.

% timestamp_to_datetime(T) ->
%     calendar:now_to_universal_time({T div 1000000,T rem 1000000,0}).
