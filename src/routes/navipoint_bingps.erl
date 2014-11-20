%% -*- coding: utf-8 -*-
-module(navipoint_bingps).

-export([init/2, post/2]).

init(Req, Opts) ->
    {navipoint_handler, Req, Opts}.

post(Body, #{skey := Skey}) ->
    repr(parsebingps(Skey, Body)).

repr({ok, Last}) ->
    % Dynamic = [{lastping, unixtime()}, {csq, Csq}] ++ navipoint:point_to_doc(Last),
    #{dt := DT} = Dynamic = navipoint:point_to_doc(Last),
    TRESHOLD = 1356991200,        % 01/01/2013

    if
        DT >= TRESHOLD ->
            #{response => <<"BINGPS: OK\r\n">>, dynamic => Dynamic};
        true ->
            % ?WARNING("===================== Skip -44 year pont ==================="),
            #{response => <<"BINGPS: OK\r\n">>, dynamic => #{error => <<"treshold">>}}
    end;

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
