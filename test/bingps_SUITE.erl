-module(bingps_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

suite() ->
    [{timetrap,{minutes,1}}].

all() -> [ errors, e1, e2, e3, cclk ].

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

    io:format("~nboo Config~p ~n~n", [Config]),

    Body1 = <<16#FF, 16#E2>>,
    {200, _, <<"BINGPS: DATAERROR\r\n">>} = Resp = helper:post(Config, "/bingps", #{}, Body1),
    io:format("~nboo Resp=~p ~n~n", [Resp]),

    Doc = navidb:get(systems, Skey),
    io:format("~nboo navidb:get(systems, ~p) Doc=~p ~n~n", [Skey, Doc]),
    #{<<"dynamic">> := #{<<"error">> := <<"dataerror">>}} = Doc,% = navidb:get(systems, Skey),

    Body2 = <<0:(32*8)/little-unsigned-integer, 1, 2>>,
    {200, _, <<"BINGPS: CRCERROR\r\n">>} = helper:post(Config, "/bingps", #{}, Body2),
    #{<<"dynamic">> := #{<<"error">> := <<"crcerror">>}} = navidb:get(systems, Skey),

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
    <<"csq">>  => 21,
    <<"vout">> => 13970,
    <<"vin">>  => 4170,
    <<"dataid">> => <<"00015B86">>
  },
  {200, _, <<"BINGPS: OK\r\n">>} = helper:post(Config, "/bingps", Extra, Body),

  Skey = base64:encode(?config(imei, Config)),
  {ok, _FakeE1Packet} = navidb:get_geos(Skey, Datetime div 3600, (Datetime div 3600) + 1),
  ct:pal("FakeE1Packet = ~p~n", [_FakeE1Packet]),

  #{<<"dynamic">> := #{
      <<"alt">>  := <<"GSM6CELL">>,
      <<"dt">>  := Datetime,
      <<"mcc">> := 255,
      <<"mnc">> := 3,
      <<"lac">> := 47080,
      <<"cid0">> := 1461,
      <<"cid1">> := 1462,
      <<"cid2">> := 9461,
      <<"cid3">> := 17403,
      <<"cid4">> := 1463,
      <<"cid5">> := 28793,
      <<"cid6">> := 17401,
      <<"rxl0">> := 52,
      <<"rxl1">> := 32,
      <<"rxl2">> := 30,
      <<"rxl3">> := 26,
      <<"rxl4">> := 24,
      <<"rxl5">> := 19,
      <<"rxl6">> := 13
  }} = navidb:get(systems, Skey),

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
        <<"csq">>  => 21,
        <<"vout">> => 13970,
        <<"vin">>  => 4170,
        <<"dataid">> => <<"00015B86">>
    },
    {200, _, <<"BINGPS: OK\r\n">>} = helper:post(Config, "/bingps", Extra, Body),

    Skey = base64:encode(?config(imei, Config)),
    {ok, FakeE2Packet} = navidb:get_geos(Skey, Datetime div 3600, (Datetime div 3600) + 1),

    #{<<"dynamic">> := #{
        <<"csq">>       := 21,
        <<"dt">>        := 1409680257,
        <<"latitude">>  := 48.501829,
        <<"longitude">> := 34.623322,
        <<"vin">>       := 4.17,
        <<"vout">>      := 13.97
    }} = navidb:get(systems, Skey),

    ok.

mcc_mnc(MCC, MNC) -> ((MCC-200)*100 + MNC).

e3(Config) ->
    Datetime = 1409680257,  % 2014-09-02 17:50:57
    Latitude =  48.50183,   % 48.50183
    Longitude = 34.62332,   % 34.62332

    MCC0 = 255, MNC0 = 03, LAC0 = 16#B7E8, CID0 = 16#05B5, RXL0 = 52, TA0 = 1,
    MCC1 = 255, MNC1 = 03, LAC1 = 16#B7E8, CID1 = 16#05B6, RXL1 = 32,
    MCC2 = 255, MNC2 = 03, LAC2 = 16#B7E8, CID2 = 16#24F5, RXL2 = 30,
    MCC3 = 255, MNC3 = 03, LAC3 = 16#B7E8, CID3 = 16#43FB, RXL3 = 26,
    MCC4 = 255, MNC4 = 03, LAC4 = 16#B7E8, CID4 = 16#05B7, RXL4 = 24,
    MCC5 = 255, MNC5 = 03, LAC5 = 16#B7E8, CID5 = 16#7079, RXL5 = 19,
    MCC6 = 255, MNC6 = 03, LAC6 = 16#B7E8, CID6 = 16#43F9, RXL6 = 13,

    FakeE3Packet = <<16#FF, 16#E3,
    (mcc_mnc(MCC0, MNC0)):16, LAC0:16, CID0:16,     % | MCC+MNC   | 2 | MCC - код страны и MNC - код сети. (MCC-200)*100 + MNC |
    (mcc_mnc(MCC1, MNC1)):16, LAC1:16, CID1:16,     % | LAC       | 2 | LAC - код локальной зоны (другими словами, совокупности базовых станций, обслуживаемых одним контроллером) |
    (mcc_mnc(MCC2, MNC2)):16, LAC2:16, CID2:16,     % | CID       | 2 | CID (CellID) - идентификатор, состоит из номеров базовой станции и сектора |
    (mcc_mnc(MCC3, MNC3)):16, LAC3:16, CID3:16,
    (mcc_mnc(MCC4, MNC4)):16, LAC4:16, CID4:16,
    (mcc_mnc(MCC5, MNC5)):16, LAC5:16, CID5:16,
    (mcc_mnc(MCC6, MNC6)):16, LAC6:16, CID6:16,
    TA0, %  | TA0       | 1 | TA - Timing Advance активной вышки |
    RXL0, % | RXL       | 1 | RXL - активной вышки |
    RXL1, % | RXL1      | 1 | RXL - соседней вышки №1 |
    RXL2, % | RXL2      | 1 | RXL - соседней вышки №2 |
    RXL3, % | RXL3      | 1 | RXL - соседней вышки №3 |
    RXL4, % | RXL4      | 1 | RXL - соседней вышки №4 |
    RXL5, % | RXL5      | 1 | RXL - соседней вышки №5 |
    RXL6, % | RXL6      | 1 | RXL - соседней вышки №6 |
    Datetime:32,            % | DATETIME  | 4 | Дата+время (метка может быть не задана или иметь неточное значение)
    (trunc(Latitude * 600000)):32/signed,     % | 4 | Широта 1/10000 минут.   Поле определено, если есть текущие координаты от GPS-модуля. |
    (trunc(Longitude * 600000)):32/signed>>,  % |    60    | LONGITUDE | 4 | Долгота 1/10000 минут.  В противном случае, LATITUDE и LONGITUDE равны 0 |

    ?assertEqual(64, size(FakeE3Packet)),

    CRC = helper:crc(FakeE3Packet),
    Body = <<FakeE3Packet/binary, CRC:16/little-unsigned-integer>>,
    Extra = #{
        <<"csq">>  => 21,
        <<"vout">> => 13970,
        <<"vin">>  => 4170,
        <<"dataid">> => <<"00015B86">>
    },
    {200, _, <<"BINGPS: OK\r\n">>} = helper:post(Config, "/bingps", Extra, Body),

    Skey = base64:encode(?config(imei, Config)),
    {ok, _FakeE3Packet} = navidb:get_geos(Skey, Datetime div 3600, (Datetime div 3600) + 1),
    ct:pal("FakeE3Packet = ~p~n", [_FakeE3Packet]),

    ct:pal("Get = ~p", [navidb:get(systems, Skey)]),

    #{<<"dynamic">> := #{
        <<"alt">>  := <<"GSM6CELL">>,
        <<"dt">>  := Datetime,
        <<"latitude">> := Latitude,
        <<"longitude">> := Longitude,
        <<"cells">> := [
            #{<<"mcc">> := MCC0, <<"mnc">> := MNC0, <<"lac">> := LAC0, <<"cid">> := CID0, <<"rxl">> := RXL0, <<"ta">> := TA0},
            #{<<"mcc">> := MCC1, <<"mnc">> := MNC1, <<"lac">> := LAC1, <<"cid">> := CID1, <<"rxl">> := RXL1},
            #{<<"mcc">> := MCC2, <<"mnc">> := MNC2, <<"lac">> := LAC2, <<"cid">> := CID2, <<"rxl">> := RXL2},
            #{<<"mcc">> := MCC3, <<"mnc">> := MNC3, <<"lac">> := LAC3, <<"cid">> := CID3, <<"rxl">> := RXL3},
            #{<<"mcc">> := MCC4, <<"mnc">> := MNC4, <<"lac">> := LAC4, <<"cid">> := CID4, <<"rxl">> := RXL4},
            #{<<"mcc">> := MCC5, <<"mnc">> := MNC5, <<"lac">> := LAC5, <<"cid">> := CID5, <<"rxl">> := RXL5},
            #{<<"mcc">> := MCC6, <<"mnc">> := MNC6, <<"lac">> := LAC6, <<"cid">> := CID6, <<"rxl">> := RXL6}
        ]
    }} = navidb:get(systems, Skey),

    ok.


cclk(Config) ->
    {200, _, Resp} = helper:get(Config, "/bingps", #{<<"cmd">> => <<"CCLK">>}),
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
