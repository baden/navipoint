%% -*- coding: utf-8 -*-
-module(navipoint_bingps).
-behaviour(navipoint_sub_protocol).

-export([init/2, get/1, post/2]).

-spec init(any(), any()) -> {atom(), any(), any()}.
init(Req, Opts) ->
    {navipoint_handler, Req, Opts}.

% Возвращает текущие дату и время в формате, пригодном для установки
% часов в SIM900 через AT+CCLK="yy/MM/dd,hh:mm:ss[+-]zz"
-spec get(map:map()) -> map:map().
get(#{params := #{<<"cmd">> := <<"CCLK">>}} = _Query) ->
    % DateTime = os:timestamp(),
    {A, B, _} = Now = os:timestamp(),
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_universal_time(Now),
    Dt = (A * 1000000) + B,
    % DTstamp = <<"yy/MM/dd,hh:mm:ss+00">>,

    RespBody = list_to_binary(lists:flatten(io_lib:format(
        "CCLK: ~2.10.0B/~2.10.0B/~2.10.0B,~2.10.0B:~2.10.0B:~2.10.0B+00\r\n"
        "DT: ~8.16.0B\r\n",
        [Year-2000, Month, Day, Hour, Min, Sec, Dt]
    ))),

    #{response => RespBody, nocommands => true}.

-spec post(binary(), map:map()) -> map:map().
post(Body, #{skey := Skey}) ->
    repr(parsebingps(Skey, Body)).

repr({ok, Last}) ->
    % Dynamic = [{lastping, unixtime()}, {csq, Csq}] ++ navipoint:point_to_doc(Last),
    % #{dt := DT} =
    Dynamic = navipoint:point_to_doc(Last),
    % TRESHOLD = 1356991200,        % 01/01/2013

    #{response => <<"BINGPS: OK\r\n">>, dynamic => Dynamic};
    % if
    %     DT >= TRESHOLD ->
    %         #{response => <<"BINGPS: OK\r\n">>, dynamic => Dynamic};
    %     true ->
    %         % ?WARNING("===================== Skip -44 year pont ==================="),
    %         #{response => <<"BINGPS: OK\r\n">>, dynamic => #{error => <<"treshold">>}}
    % end;

repr(dataerror) ->
    navistats:notify(point_error_data, {inc, 1}),
    #{response => <<"BINGPS: DATAERROR\r\n">>, dynamic => #{error => <<"dataerror">>}};

repr(crcerror) ->
    navistats:notify(point_error_crc, {inc, 1}),
    #{response => <<"BINGPS: CRCERROR\r\n">>, dynamic => #{error => <<"crcerror">>}}.



parsebingps(_Skey, Body) when byte_size(Body) < 3 ->
    % spec_log(Skey, "[~p] parsebingps dataerror", [Skey]),
    dataerror;

parsebingps(Skey, Body) ->

    Size = size(Body) - 2,

    <<Data:Size/binary, CRC:16/little-unsigned-integer>> = Body,

    case navipoint:crc(Data) of
        CRC ->
            % spec_log(Skey, "[~p] CRC OK. parsedata start", [Skey]),
            parsedata(Skey, Data);
            % spec_log(Skey, "[~p] CRC OK. parsedata stop ", [Skey]);
        _ ->
            crcerror
    end.

parsedata(Skey, Data) ->
    {ok, Parsed, Last} = navipoint:parse(Data),

    Keys = lists:sort(dict:fetch_keys(Parsed)),

    [navidb_gpsdb:save(Skey, Hour, erlang:list_to_binary(dict:fetch(Hour, Parsed))) || Hour <- Keys],

    {ok, Last}.

% unixtime() ->
%     timer:now_diff(now(), {0,0,0}) div 1000000.
% unixtime() ->
%         {A, B, _} = os:timestamp(),
%         (A * 1000000) + B.
