%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%% @copyright Denis Batrak
%% @author Batrak Denis <baden.i.ua@gmail.com>
%% @version {@vsn}, {@date} {@time}
%% @doc ErlNaviCC tracker point
%% @end
%%%-------------------------------------------------------------------
-module(navipoint).

%% API
-export([start/0, stop/0]).

%%====================================================================
%% API
%%====================================================================

-export([point_to_doc/1, parse/1, crc/1]).
-export([iso_8601_fmt/1]).

%% @spec start() -> ok
%% @doc Start the pymwyfa_web server.
% Manual start over -s navipoint
-spec start() -> ok.
start() ->
  application:load(navipoint),

  {ok, Apps} = application:get_key(navipoint, applications),
  [application:ensure_all_started(App) || App <- Apps],
  ok = application:start(navipoint),
  ok.

%% @spec stop() -> ok
%% @doc Stop the pymwyfa_web server.
-spec stop() -> ok | {error, term()}.
stop() ->
  Res = application:stop(navipoint),
  Res.

% a = 123l;


% Необходимо разобрать пакет данных на пачки по 1му часу в каждой.
% Используемые параметры:
% Bin - разбираемый пакет
% Текущий час (undefined) вначале разбора
% Аккумулятор одного часа
% Общий аккумулятор для функций update

-spec parse(binary()) -> {ok, list(), dict:dict()}.
parse(Bin) when is_binary(Bin) ->
    parse(Bin, undefined, [], dict:new()).

parse(Full = <<255, 16#F5,       % Пакет 0xF5
    _:2/binary,                             % Выделим одно поле:
    DATETIME:32/little-unsigned-integer,    % Дата+время (unixtime)
    _:24/binary,
    Rest/binary>>, _Last, Line, Acc) ->

    Hour = DATETIME div 3600,
    % ?INFO("BLOCK 0xF5: dt = ~p (~p)", [DATETIME, iso_8601_fmt(DATETIME)]),

    << Block:32/binary, _/binary >> = Full,

    parse(Rest, Block, Line, dict:append(Hour, Block, Acc));

% TODO: Возможно лучшим решением будет сделать преобразование протокола
parse(Full = <<255, 16#F2, _:30/binary,       % Пакет 0xF2 преобразовываем в #F5
    Rest/binary>>, Last, Line, Acc) ->

    % Block = <<255, 16#F5, Skip1, DAY, M_Y, HOUR, MINUTE, SECOND, Skip2:192>>,
    << OldBlock:32/binary, _/binary >> = Full,

    {Block, DATETIME, Valid} = convert(OldBlock),  % Попутно получим DATETIME. Третий параметр будет установлен в false если точка не вылидна.

    case DATETIME of
        0 ->
            parse(Rest, Last, Line, Acc);
        _ ->
            Hour = DATETIME div 3600,
            % ?INFO("BLOCK 0xF2: dt = ~p (~p)", [DATETIME, iso_8601_fmt(DATETIME)]),
            case Valid of
                false ->
                    parse(Rest, Last, Line, dict:append(Hour, Block, Acc));
                _ ->
                    parse(Rest, Block, Line, dict:append(Hour, Block, Acc))
            end
    end;

parse(Full = <<255, 16#E1,       % Пакет 0xE1 - Определение положения по сотовым вышкам (примерное)
    _MCC_MNC:16/little-unsigned-integer, % | MCC+MNC   | 2 | MCC - код страны и MNC - код сети. (MCC-200)*100 + MNC |
    _LAC:16/little-unsigned-integer,     % | LAC       | 2 | LAC - код локальной зоны (другими словами, совокупности базовых станций, обслуживаемых одним контроллером) |
    _CID0:16/little-unsigned-integer,    % | CID       | 2 | CID (CellID) - идентификатор, состоит из номеров базовой станции и сектора |
    DATETIME:32/little-unsigned-integer,% | DATETIME  | 4 | Дата+время (метка может быть не задана или иметь неточное значение)
    _CID1:16/little-unsigned-integer,    % | CID1      | 2 | CID - соседней вышки №1 |
    _CID2:16/little-unsigned-integer,    % | CID2      | 2 | CID - соседней вышки №2 |
    _CID3:16/little-unsigned-integer,    % | CID3      | 2 | CID - соседней вышки №3 |
    _CID4:16/little-unsigned-integer,    % | CID4      | 2 | CID - соседней вышки №4 |
    _CID5:16/little-unsigned-integer,    % | CID5      | 2 | CID - соседней вышки №5 |
    _CID6:16/little-unsigned-integer,    % | CID6      | 2 | CID - соседней вышки №6 |
    _RXL0,   % | RXL       | 1 | RXL - активной вышки |
    _RXL1,   % | RXL1      | 1 | RXL - соседней вышки №1 |
    _RXL2,   % | RXL2      | 1 | RXL - соседней вышки №2 |
    _RXL3,   % | RXL3      | 1 | RXL - соседней вышки №3 |
    _RXL4,   % | RXL4      | 1 | RXL - соседней вышки №4 |
    _RXL5,   % | RXL5      | 1 | RXL - соседней вышки №5 |
    _RXL6,   % | RXL6      | 1 | RXL - соседней вышки №6 |
    _,      % | CRC (res) | 1 | ? |
    Rest/binary>>, _Last, Line, Acc) ->
        Hour = DATETIME div 3600,
        % MCC = (MCC_MNC div 100) + 200,
        % MNC = MCC_MNC rem 100,
        % ct:pal("BLOCK 0xE1: dt = ~p (~p)~n"
        %     "  MCC = ~p~n"
        %     "  MNC = ~p~n"
        %     "  LAC = ~p~n"
        %     "  CID0 = ~p~n"
        %     "  CID1 = ~p~n"
        %     "  CID2 = ~p~n"
        %     "  CID3 = ~p~n"
        %     "  CID4 = ~p~n"
        %     "  CID5 = ~p~n"
        %     "  CID6 = ~p~n"
        %     "  RXL0 = ~p~n"
        %     "  RXL1 = ~p~n"
        %     "  RXL2 = ~p~n"
        %     "  RXL3 = ~p~n"
        %     "  RXL4 = ~p~n"
        %     "  RXL5 = ~p~n"
        %     "  RXL6 = ~p~n"
        %     , [
        %         DATETIME, iso_8601_fmt(DATETIME),
        %         MCC, MNC, LAC,
        %         CID0, CID1, CID2, CID3, CID4, CID5, CID6,
        %         RXL0, RXL1, RXL2, RXL3, RXL4, RXL5, RXL6
        %     ]
        % ),
        % ct:pal("BLOCK 0xE2: Latitude = ~p", [Latitude]),
        % ct:pal("BLOCK 0xE2: Longitude = ~p", [Longitude]),

        << Block:32/binary, _/binary >> = Full,

        parse(Rest, Block, Line, dict:append(Hour, Block, Acc));


parse(Full = <<255, 16#E2,       % Пакет 0xE2 - Определение положения по сотовым вышкам (примерное)
    _:2/binary,                             % Res1
    DATETIME:32/little-unsigned-integer,    % Дата+время (unixtime)
    _Latitude:32/little-unsigned-integer,    % Широта
    _Longitude:32/little-unsigned-integer,   % Долгота
    _:16/binary,
    Rest/binary>>, _Last, Line, Acc) ->

    Hour = DATETIME div 3600,
    % ct:pal("BLOCK 0xE2: dt = ~p (~p)", [DATETIME, iso_8601_fmt(DATETIME)]),
    % ct:pal("BLOCK 0xE2: Latitude = ~p", [Latitude]),
    % ct:pal("BLOCK 0xE2: Longitude = ~p", [Longitude]),

    << Block:32/binary, _/binary >> = Full,

    parse(Rest, Block, Line, dict:append(Hour, Block, Acc));
    % parse(Rest, Last, Line, dict:append(Hour, Block, Acc));
    % parse(Rest, Last, Line, Acc);

% Пакет 0xE3 - Определение положения по сотовым вышкам (расширенная)
parse(Full = <<255, 16#E3,              % | HEAD      | 1 | Заголовок пакета. Всегда имеет значение 0xFF. |
                                        % | ID        | 1 | Идентификатор пакета. Имеет значение 0xE3 |
    _MCC_MNC0:16, _LAC0:16, _CID0:16,   % | MCCx+MNCx | 2 | MCC - код страны и MNC - код сети. (MCC-200)*100 + MNC вышки №x |
    _MCC_MNC1:16, _LAC1:16, _CID1:16,   % | LACx      | 2 | LAC - код локальной зоны вышки №x |
    _MCC_MNC2:16, _LAC2:16, _CID2:16,   % | CIDx      | 2 | CID (CellID) - идентификатор, состоит из номеров базовой станции и сектора |
    _MCC_MNC3:16, _LAC3:16, _CID3:16,
    _MCC_MNC4:16, _LAC4:16, _CID4:16,
    _MCC_MNC5:16, _LAC5:16, _CID5:16,
    _MCC_MNC6:16, _LAC6:16, _CID6:16,
    _TA0,    % | TA0       | 1 | TA - Timing Advance (задержка сигнала?) активной вышки |
    _RXL0,   % | RXL       | 1 | RXL - активной вышки |
    _RXL1,   % | RXL1      | 1 | RXL - соседней вышки №1 |
    _RXL2,   % | RXL2      | 1 | RXL - соседней вышки №2 |
    _RXL3,   % | RXL3      | 1 | RXL - соседней вышки №3 |
    _RXL4,   % | RXL4      | 1 | RXL - соседней вышки №4 |
    _RXL5,   % | RXL5      | 1 | RXL - соседней вышки №5 |
    _RXL6,   % | RXL6      | 1 | RXL - соседней вышки №6 |
    DATETIME:32,            % | DATETIME  | 4 | Дата+время (метка может быть не задана или иметь неточное значение)
    _LATITUDE:32/signed,     % | LATITUDE | 4 | Широта 1/10000 минут.   Поле определено, если есть текущие координаты от GPS-модуля. |
    _LONGITIDE:32/signed,    % | LONGITUDE | 4 | Долгота 1/10000 минут.  В противном случае, LATITUDE и LONGITUDE равны 0 |
    Rest/binary>>, _Last, Line, Acc) ->
        Hour = DATETIME div 3600,
        << Block:64/binary, _/binary >> = Full,
        parse(Rest, Block, Line, dict:append(Hour, Block, Acc));

parse(<<_Char, Rest/binary>>, Last, Line, Acc) ->  % Поиск 0xFF
    % ?INFO("skip ~.16B", [_Char]),
    parse(Rest, Last, Line, Acc);

parse(<<>>, Last, [], Acc) ->
    % ?INFO("end"),
    {ok, Acc, Last}.



% iso_8601_fmt(DateTime) ->
%     {{Year,Month,Day},{Hour,Min,Sec}} = timestamp_to_datetime(DateTime),
%     lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
%         [Year, Month, Day, Hour, Min, Sec])).
%
% timestamp_to_datetime(T) ->
%     calendar:now_to_universal_time({T div 1000000,T rem 1000000,0}).

%
% Получение записи из бинарного пакета.
% Применяется для получения JSON-ответа для поля dynamic
%
-spec point_to_doc(binary()) -> {ok, map()}.
point_to_doc(<<255, 16#F5,          % Пакет 0xF5
    FSOURCE,                                % Тип точки. Причина фиксации точки (младшие 6 бит), бит7 - фиксация без активных спутников.
    SATS,                                   % Кол-во спутников 3..12
    DATETIME:32/little-unsigned-integer,    % Дата+время (unixtime)
    LATITUDE:32/little-signed-integer,      % Широта 1/10000 минут
    LONGITIDE:32/little-signed-integer,     % Долгота 1/10000 минут
    SPEED:16/little-unsigned-integer,       % Скорость 1/100 узла
    ALTITUDE:16/little-signed-integer,      % Высота над уровнем моря, метров (-30000…30000)
    DIR,                                    % Направление/2 = 0..179
    VOUT,                                   % Напряжение внешнего питания 1/10 B (0.0-102.3) старшие 8 бит.
    VIN,                                    % Напряжение внутреннего аккумулятора 1/100 B (0.00-10.23) старшие 8 бит
    ADC1,                                   % АЦП1 (Температура?) старшие 8 бит
    ADC2,                                   % АЦП2 (Уровень топлива) старшие 8 бит
    ADC2L:2, ADC1L:2, VINL:2, VOUTL:2,      % Младщие биты полей VOUT, VIN, ADC1, ADC2 (4 x 2 бита)
    RES1,                                   % Резерв (АЦП3 (Вход 3))
    RES2,                                   % Резерв (АЦП1..3lsb (младшие биты: 3 x 2 младших бита))
    RES3,                                   % Резерв
    RES4,                                   % Резерв
    RES5,                                   % Резерв
    _LCRC>> = Packet) ->                    % Локальная CRC (сумма всех байтов пакета, младший разряд)

    % TODO! Добавить проверку LCRC после реализакии в трекерах
    % ?INFO("ADC_LSB = ~w", [ADC_LSB]),

    #{
        fsource     => FSOURCE,
        sats        => SATS,
        dt          => DATETIME,
        latitude    => LATITUDE / 600000.0,
        longitude   => LONGITIDE / 600000.0,
        speed       => SPEED * 1.852 / 100.0,
        alt         => ALTITUDE,
        course      => DIR * 2,
        vout        => (VOUT * 4 + VOUTL) / 10.0,
        vin         => (VIN * 4 + VINL) / 100.0,
        fuel        => ADC2 * 4 + ADC2L,
        adc1        => ADC1 * 4 + ADC1L,
        res1        => RES1,
        res2        => RES2,
        res3        => RES3,
        res4        => RES4,
        res5        => RES5,
        raw         => binary_to_list(Packet)
    };

point_to_doc(<<255, 16#E1,          % Пакет 0xE1 - Определение положения по сотовым вышкам (примерное)
    MCC_MNC:16/little-unsigned-integer, % | MCC+MNC   | 2 | MCC - код страны и MNC - код сети. (MCC-200)*100 + MNC |
    LAC:16/little-unsigned-integer,     % | LAC       | 2 | LAC - код локальной зоны (другими словами, совокупности базовых станций, обслуживаемых одним контроллером) |
    CID0:16/little-unsigned-integer,    % | CID       | 2 | CID (CellID) - идентификатор, состоит из номеров базовой станции и сектора |
    DATETIME:32/little-unsigned-integer,% | DATETIME  | 4 | Дата+время (метка может быть не задана или иметь неточное значение)
    CID1:16/little-unsigned-integer,    % | CID1      | 2 | CID - соседней вышки №1 |
    CID2:16/little-unsigned-integer,    % | CID2      | 2 | CID - соседней вышки №2 |
    CID3:16/little-unsigned-integer,    % | CID3      | 2 | CID - соседней вышки №3 |
    CID4:16/little-unsigned-integer,    % | CID4      | 2 | CID - соседней вышки №4 |
    CID5:16/little-unsigned-integer,    % | CID5      | 2 | CID - соседней вышки №5 |
    CID6:16/little-unsigned-integer,    % | CID6      | 2 | CID - соседней вышки №6 |
    RXL0,   % | RXL       | 1 | RXL - активной вышки |
    RXL1,   % | RXL1      | 1 | RXL - соседней вышки №1 |
    RXL2,   % | RXL2      | 1 | RXL - соседней вышки №2 |
    RXL3,   % | RXL3      | 1 | RXL - соседней вышки №3 |
    RXL4,   % | RXL4      | 1 | RXL - соседней вышки №4 |
    RXL5,   % | RXL5      | 1 | RXL - соседней вышки №5 |
    RXL6,   % | RXL6      | 1 | RXL - соседней вышки №6 |
    _>> = Packet) ->  % | CRC (res) | 1 | ? |

    MCC = (MCC_MNC div 100) + 200,
    MNC = MCC_MNC rem 100,

    #{
        alt         => <<"GSM6CELL">>,
        dt          => DATETIME,
        mcc         => MCC,
        mnc         => MNC,
        lac         => LAC,
        cid0        => CID0,
        cid1        => CID1,
        cid2        => CID2,
        cid3        => CID3,
        cid4        => CID4,
        cid5        => CID5,
        cid6        => CID6,
        rxl0        => RXL0,
        rxl1        => RXL1,
        rxl2        => RXL2,
        rxl3        => RXL3,
        rxl4        => RXL4,
        rxl5        => RXL5,
        rxl6        => RXL6,
        raw         => binary_to_list(Packet)
    };

point_to_doc(<<255, 16#E3,   % Пакет 0xE3 - Определение положения по сотовым вышкам (примерное)
    % | MCC+MNC   | 2 | MCC - код страны и MNC - код сети. (MCC-200)*100 + MNC |
    % | LAC       | 2 | LAC - код локальной зоны (другими словами, совокупности базовых станций, обслуживаемых одним контроллером) |
    % | CID       | 2 | CID (CellID) - идентификатор, состоит из номеров базовой станции и сектора |
    MCC_MNC0:16, LAC0:16, CID0:16,
    MCC_MNC1:16, LAC1:16, CID1:16,
    MCC_MNC2:16, LAC2:16, CID2:16,
    MCC_MNC3:16, LAC3:16, CID3:16,
    MCC_MNC4:16, LAC4:16, CID4:16,
    MCC_MNC5:16, LAC5:16, CID5:16,
    MCC_MNC6:16, LAC6:16, CID6:16,
    TA0,    % | TA0       | 1 | TA - активной вышки |
    RXL0,   % | RXL       | 1 | RXL - активной вышки |
    RXL1,   % | RXL1      | 1 | RXL - соседней вышки №1 |
    RXL2,   % | RXL2      | 1 | RXL - соседней вышки №2 |
    RXL3,   % | RXL3      | 1 | RXL - соседней вышки №3 |
    RXL4,   % | RXL4      | 1 | RXL - соседней вышки №4 |
    RXL5,   % | RXL5      | 1 | RXL - соседней вышки №5 |
    RXL6,   % | RXL6      | 1 | RXL - соседней вышки №6 |
    DATETIME:32,            % | DATETIME  | 4 | Дата+время (метка может быть не задана или иметь неточное значение)
    LATITUDE:32/signed,     % | LATITUDE | 4 | Широта 1/10000 минут.   Поле определено, если есть текущие координаты от GPS-модуля. |
    LONGITIDE:32/signed     % | LONGITUDE | 4 | Долгота 1/10000 минут.  В противном случае, LATITUDE и LONGITUDE равны 0 |
    >> = Packet) ->

    MCC0 = (MCC_MNC0 div 100) + 200, MNC0 = MCC_MNC0 rem 100,
    MCC1 = (MCC_MNC1 div 100) + 200, MNC1 = MCC_MNC1 rem 100,
    MCC2 = (MCC_MNC2 div 100) + 200, MNC2 = MCC_MNC2 rem 100,
    MCC3 = (MCC_MNC3 div 100) + 200, MNC3 = MCC_MNC3 rem 100,
    MCC4 = (MCC_MNC4 div 100) + 200, MNC4 = MCC_MNC4 rem 100,
    MCC5 = (MCC_MNC5 div 100) + 200, MNC5 = MCC_MNC5 rem 100,
    MCC6 = (MCC_MNC6 div 100) + 200, MNC6 = MCC_MNC6 rem 100,

    #{
        alt         => <<"GSM6CELL">>,
        dt          => DATETIME,
        latitude    => LATITUDE / 600000.0,
        longitude   => LONGITIDE / 600000.0,
        cells       => [
            #{mcc => MCC0, mnc => MNC0, lac => LAC0, cid => CID0, rxl => RXL0, ta => TA0},
            #{mcc => MCC1, mnc => MNC1, lac => LAC1, cid => CID1, rxl => RXL1},
            #{mcc => MCC2, mnc => MNC2, lac => LAC2, cid => CID2, rxl => RXL2},
            #{mcc => MCC3, mnc => MNC3, lac => LAC3, cid => CID3, rxl => RXL3},
            #{mcc => MCC4, mnc => MNC4, lac => LAC4, cid => CID4, rxl => RXL4},
            #{mcc => MCC5, mnc => MNC5, lac => LAC5, cid => CID5, rxl => RXL5},
            #{mcc => MCC6, mnc => MNC6, lac => LAC6, cid => CID6, rxl => RXL6}
        ],
        raw         => binary_to_list(Packet)
    };


point_to_doc(<<255, 16#E2,          % Пакет 0xE2 - Определение положения по сотовым вышкам (примерное)
    _:2/binary,                             % Res1
    DATETIME:32/little-unsigned-integer,    % Дата+время (unixtime)
    LATITUDE:32/little-signed-integer,    % Широта
    LONGITIDE:32/little-signed-integer,   % Долгота
    _:16/binary>> = Packet) ->                       % Res2

    #{
        alt         => <<"GSMCELL">>,
        dt          => DATETIME,
        latitude    => LATITUDE / 1000000.0,
        longitude   => LONGITIDE / 1000000.0,
        res0        => 0,
        raw         => binary_to_list(Packet)
    };

point_to_doc(undefined) ->
    #{
        fsource     => 0,
        sats        => 0,
        dt          => 0,
        latitude    => 0.0,
        longitude   => 0.0,
        speed       => 0.0,
        alt         => 0,
        course      => 0,
        vout        => 0.0,
        vin         => 0.0,
        fuel        => 0.0,
        adc1        => 0.0,
        res1        => 0,
        res2        => 0,
        res3        => 0,
        res4        => 0,
        res5        => 0,
        raw         => []
    };

point_to_doc(Packet) ->
    #{
        fsource     => 0,
        sats        => 0,
        dt          => 0,
        latitude    => 0.0,
        longitude   => 0.0,
        speed       => 0.0,
        alt         => 0,
        course      => 0,
        vout        => 0.0,
        vin         => 0.0,
        fuel        => 0.0,
        adc1        => 0.0,
        res1        => 0,
        res2        => 0,
        res3        => 0,
        res4        => 0,
        res5        => 0,
        raw         => binary_to_list(Packet)
    }.


% Используется для преобразования устаревшего протокола F2 в новый F5
convert(<<16#FF, 16#F2,           % Пакет 0xF2 (Устаревшее)
    _LEN,                                   % Длина пакета в байтах, включая HEADER, ID и LENGTH. Имеет значение 32
    DAY,                                    % День месяца = 1..31
    Y:4, M:4,                               % месяц | ((год-2010) « 4). Месяц = 1..12 год = 0..14 → 2010..2024
    HOUR,                                   % Часы = 0..23
    MINUTE,                                 % MINUTE  1   Минуты = 0..59
    SECOND,                                 % SECOND  1   Cекунды = 0..59
    LAT1,                                   % Широта (LL) Градусы широты = 0..89
    LAT2,                                   % Широта (ll) Минуты целая часть = 0..59
    LAT3,                                   % Широта (mm) Минуты дробная часть1 = 0..99
    LAT4,                                   % Широта (nn) Минуты дробная часть2 = 0..99
    LON1,                                   % Долгота (LLL) Градусы долготы = 0..179
    LON2,                                   % Долгота (ll) Минуты целая часть = 0..59
    LON3,                                   % Долгота (mm) Минуты дробная часть1 = 0..99
    LON4,                                   % Долгота (nn) Минуты дробная часть2 = 0..99
    _EXT:6, EW:1, NS:1,                     % бит0 = NS бит1 = EW бит2 = (DIR & 1). бит0=0 для N, бит0=1 для S, бит1=0 для E, бит1=1 для W, бит2=0 для четных DIR, бит2=1 для нечетных DIR
    SATS,                                   % Кол-во спутников 3..12
    SPEEDP,                                 % Скорость в узлах 0..239
    SPEEDF,                                 % Скорость дробная часть Дробная часть скорости 0..99
    DIR,                                    % Направление/2 = 0..179
    _DIRF,                                  % Дробная часть направления 0..99
    VOUT1:16/little-unsigned-integer,       % Напряжение внешнего питания, U/100 = 0..2000
    VIN:16/little-unsigned-integer,         % Напряжение внутреннего аккумулятора, U/100 = 0..5000
    RES1,                                   % Зарезервировано = 0
    FSOURCE,                                % Тип точки. Причина фиксации точки
    _TDELTA:16/little-unsigned-integer,      % Неточное смещение. Смещение относительно точного времени в секундах. Значение 0xFFFF означает превышение лимита и должно игнорироваться если это возможно.
    RES2:16/little-unsigned-integer         % Зарезервировано. Используется для передачи дополнительного значения (фотодатчик, HDOP, уровень топлива)
    >>)

    % DAY,Y,M,HOUR,MINUTE,SECOND,LAT1,LAT2,LAT3,LAT4,LON1,LON2,LON3,LON4,SATS,SPEEDP,SPEEDF,DIR,VOUT1,VIN,RES1,FSOURCE
    when
        DAY >= 1, DAY =< 31,
        Y >= 3, Y =< 13,
        HOUR < 24,
        MINUTE < 60,
        SECOND < 60
    ->

    % ?INFO("convert2 ~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,", [DAY,Y,M,HOUR,MINUTE,SECOND,LAT1,LAT2,LAT3,LAT4,LON1,LON2,LON3,LON4,SATS,SPEEDP,SPEEDF,DIR,VOUT1,VIN,RES1,FSOURCE]),
    % DAY,Y,M,HOUR,MINUTE,SECOND,LAT1,LAT2,LAT3,LAT4,LON1,LON2,LON3,LON4,SATS,SPEEDP,SPEEDF,DIR,VOUT1,VIN,RES1,FSOURCE
    %   4,2,0,   2,    51,     4,  48,  16,  68,  16,  32,  32,   0,  12,   0,     0,     0,  0, 1314,416,   0,      0,

    Year = Y + 2010,
    DATETIME = datetime_to_unixtime({{Year, M, DAY}, {HOUR, MINUTE, SECOND}}),
    % ?INFO("DATETIME = ~p", [DATETIME]),
    % TODO! Добавить проверку LCRC после реализакии в трекерах
    % ?INFO("ADC_LSB = ~w", [ADC_LSB]),
    LATITUDE1 = (LAT1 * 60 + LAT2) * 10000 + LAT3 * 100 + LAT4,
    LONGITIDE1 = (LON1 * 60 + LON2) * 10000 + LON3 * 100 + LON4,

    LATITUDE = case NS of
        1 -> - LATITUDE1;
        _ -> LATITUDE1
    end,
    LONGITIDE = case EW of
        1 -> -LONGITIDE1;
        _ -> LONGITIDE1
    end,

    SPEED = SPEEDP * 100 + SPEEDF,
    ALTITUDE = 0,
    VOUT = (VOUT1 + 9) div 10,
    FUEL = RES2 div 2,

    % ?INFO("VIN = ~w (~w)", [VIN, <<(VIN bsr 2), FUEL:2, 0:2, VIN:2, VOUT:2>>]),

    Block = <<
        16#FF, 16#F5,
        FSOURCE,                                % Тип точки. Причина фиксации точки (младшие 6 бит), бит7 - фиксация без активных спутников.
        SATS,                                   % Кол-во спутников 3..12
        DATETIME:32/little-unsigned-integer,    % Дата+время (unixtime)
        LATITUDE:32/little-unsigned-integer,    % Широта 1/10000 минут
        LONGITIDE:32/little-unsigned-integer,   % Долгота 1/10000 минут
        SPEED:16/little-unsigned-integer,       % Скорость 1/100 узла
        ALTITUDE:16/little-unsigned-integer,    % Высота над уровнем моря, метров (-30000…30000)
        DIR,                                    % Направление/2 = 0..179
        (VOUT bsr 2),                           % Напряжение внешнего питания 1/10 B (0.0-102.3) старшие 8 бит.
        (VIN bsr 2),                            % Напряжение внутреннего аккумулятора 1/100 B (0.00-10.23) старшие 8 бит
        0,                                      % АЦП1 (Температура?) старшие 8 бит
        (FUEL bsr 2),                           % АЦП2 (Уровень топлива) старшие 8 бит
        FUEL:2, 0:2, VIN:2, VOUT:2,             % Младщие биты полей VOUT, VIN, ADC1, ADC2 (4 x 2 бита)
        0,                                      % Резерв (АЦП3 (Вход 3))
        0,                                      % Резерв (АЦП1..3lsb (младшие биты: 3 x 2 младших бита))
        RES1,                                   % Резерв
        0,                                      % Резерв
        0,                                      % Резерв
        0                                       % TODO: Расчитать CRC
    >>,

    case (FSOURCE band 128) of
        128 ->
            {Block, DATETIME, false};
        _ ->
            {Block, DATETIME, true}
    end;

convert(_) ->
    % ?INFO("convert: wrong packet. TBD! Skip for now"),
    lager:warning("convert: wrong packet. TBD! Skip for now", []),
    {<<0:(8*32)>>, 0, false}.


% -compile({inline, [{datetime_to_unixtime, 1}]}).
datetime_to_unixtime(Datetime) ->
    try calendar:datetime_to_gregorian_seconds( Datetime ) - 62167219200 of
        Value -> Value
    catch
        _:_ -> 0
    end.


-define(CRC16_CCITT_table, [
    16#0000, 16#1021, 16#2042, 16#3063, 16#4084, 16#50a5, 16#60c6, 16#70e7, 16#8108, 16#9129, 16#a14a, 16#b16b,
    16#c18c, 16#d1ad, 16#e1ce, 16#f1ef, 16#1231, 16#0210, 16#3273, 16#2252, 16#52b5, 16#4294, 16#72f7, 16#62d6,
    16#9339, 16#8318, 16#b37b, 16#a35a, 16#d3bd, 16#c39c, 16#f3ff, 16#e3de, 16#2462, 16#3443, 16#0420, 16#1401,
    16#64e6, 16#74c7, 16#44a4, 16#5485, 16#a56a, 16#b54b, 16#8528, 16#9509, 16#e5ee, 16#f5cf, 16#c5ac, 16#d58d,
    16#3653, 16#2672, 16#1611, 16#0630, 16#76d7, 16#66f6, 16#5695, 16#46b4, 16#b75b, 16#a77a, 16#9719, 16#8738,
    16#f7df, 16#e7fe, 16#d79d, 16#c7bc, 16#48c4, 16#58e5, 16#6886, 16#78a7, 16#0840, 16#1861, 16#2802, 16#3823,
    16#c9cc, 16#d9ed, 16#e98e, 16#f9af, 16#8948, 16#9969, 16#a90a, 16#b92b, 16#5af5, 16#4ad4, 16#7ab7, 16#6a96,
    16#1a71, 16#0a50, 16#3a33, 16#2a12, 16#dbfd, 16#cbdc, 16#fbbf, 16#eb9e, 16#9b79, 16#8b58, 16#bb3b, 16#ab1a,
    16#6ca6, 16#7c87, 16#4ce4, 16#5cc5, 16#2c22, 16#3c03, 16#0c60, 16#1c41, 16#edae, 16#fd8f, 16#cdec, 16#ddcd,
    16#ad2a, 16#bd0b, 16#8d68, 16#9d49, 16#7e97, 16#6eb6, 16#5ed5, 16#4ef4, 16#3e13, 16#2e32, 16#1e51, 16#0e70,
    16#ff9f, 16#efbe, 16#dfdd, 16#cffc, 16#bf1b, 16#af3a, 16#9f59, 16#8f78, 16#9188, 16#81a9, 16#b1ca, 16#a1eb,
    16#d10c, 16#c12d, 16#f14e, 16#e16f, 16#1080, 16#00a1, 16#30c2, 16#20e3, 16#5004, 16#4025, 16#7046, 16#6067,
    16#83b9, 16#9398, 16#a3fb, 16#b3da, 16#c33d, 16#d31c, 16#e37f, 16#f35e, 16#02b1, 16#1290, 16#22f3, 16#32d2,
    16#4235, 16#5214, 16#6277, 16#7256, 16#b5ea, 16#a5cb, 16#95a8, 16#8589, 16#f56e, 16#e54f, 16#d52c, 16#c50d,
    16#34e2, 16#24c3, 16#14a0, 16#0481, 16#7466, 16#6447, 16#5424, 16#4405, 16#a7db, 16#b7fa, 16#8799, 16#97b8,
    16#e75f, 16#f77e, 16#c71d, 16#d73c, 16#26d3, 16#36f2, 16#0691, 16#16b0, 16#6657, 16#7676, 16#4615, 16#5634,
    16#d94c, 16#c96d, 16#f90e, 16#e92f, 16#99c8, 16#89e9, 16#b98a, 16#a9ab, 16#5844, 16#4865, 16#7806, 16#6827,
    16#18c0, 16#08e1, 16#3882, 16#28a3, 16#cb7d, 16#db5c, 16#eb3f, 16#fb1e, 16#8bf9, 16#9bd8, 16#abbb, 16#bb9a,
    16#4a75, 16#5a54, 16#6a37, 16#7a16, 16#0af1, 16#1ad0, 16#2ab3, 16#3a92, 16#fd2e, 16#ed0f, 16#dd6c, 16#cd4d,
    16#bdaa, 16#ad8b, 16#9de8, 16#8dc9, 16#7c26, 16#6c07, 16#5c64, 16#4c45, 16#3ca2, 16#2c83, 16#1ce0, 16#0cc1,
    16#ef1f, 16#ff3e, 16#cf5d, 16#df7c, 16#af9b, 16#bfba, 16#8fd9, 16#9ff8, 16#6e17, 16#7e36, 16#4e55, 16#5e74,
    16#2e93, 16#3eb2, 16#0ed1, 16#1ef0
]).

-spec crc(binary()) -> {ok, non_neg_integer()}.
crc(List) ->
   crc(List,16#0000).

crc(<<>>,CRC) ->
   CRC;

crc([],CRC) ->
   CRC;

crc(<<Value:8,Rest/binary>>,CRC) when Value =< 255->
    Index = ((CRC bsr 8) bxor (16#ff band Value)),
    NewCRC = (((CRC bsl 8) band 16#ff00) bxor crc_index(Index)),
    crc(Rest,NewCRC);

crc([Value|Rest],CRC) when Value =< 255->
    Index = ((CRC bsr 8) bxor (16#ff band Value)),
    NewCRC = (((CRC bsl 8) band 16#ff00) bxor crc_index(Index)),
    crc(Rest,NewCRC).

crc_index(N) ->
   lists:nth(N+1,?CRC16_CCITT_table).


%%====================================================================
%% Internal functions
%%====================================================================


-spec iso_8601_fmt(non_neg_integer()) -> string().
iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = timestamp_to_datetime(DateTime),
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec])).

timestamp_to_datetime(T) ->
    calendar:now_to_universal_time({T div 1000000,T rem 1000000,0}).
