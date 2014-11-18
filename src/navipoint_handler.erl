%% -*- coding: utf-8 -*-
-module(navipoint_handler).

% -behaviour(cowboy_sub_protocol).

-export([
        upgrade/6
    ]).

-export([terminate/3]).

% -export([
%     % getSystem/1,
%     % getOptions/1,
%     % getImei/1
%     ]).

% -type proplist() :: list({term(), term()}).

% -record(state, {
%                 method      :: binary(),
%                 body        :: proplist(),
%                 params      :: proplist(),
%                 querystring :: proplist(),
%                 options     :: proplist(),
%                 handler     :: module(),
%                 imei        :: binary(),
%                 skey        :: binary(),
%                 dynamic     :: map(),
%                 system      :: proplist() | atom()
%     }).

% getSystem(#state{system = System}) -> System.
% getOptions(#state{options = Options}) -> Options.
% getImei(#state{imei = Imei}) -> Imei.

upgrade(Req, Env, Handler, _HandlerState, infinity, run) ->
% upgrade(Req, Env, Handler, Opts) ->
    folsom_metrics:notify(point, {inc, 1}),
    folsom_metrics:notify(point_meter, 1),
    Begin = folsom_metrics:histogram_timed_begin(point_duration),

    % Start = now(),
    % Params = cowboy_req:bindings(Req),
    % Method = cowboy_req:method(Req),
    % Query = cowboy_req:parse_qs(Req),

    Method = cowboy_req:method(Req),
    % Headers = maps:from_list(cowboy_req:headers(Req)),
    Params = maps:from_list(cowboy_req:bindings(Req)),
    Query  = maps:from_list(cowboy_req:parse_qs(Req)),

    Csq =
        try
            erlang:binary_to_integer(maps:get(<<"csq">>, Query, <<"0">>))
        catch
            error:badarg -> 0
        end,

    DynamicInit = #{
        lastping => unixtime(),
        method   => Method,
        csq      => Csq
    },

    % Все входящие сообщения должны содержать параметр imei. Иначе отбой соединения
    case maps:get(<<"imei">>, Query, undefined) of
        undefined ->
            {ok, Req4} = cowboy_req:reply(500, [{<<"content-type">>, <<"application/octet-stream">>}], <<"ERROR: imei query must be defined">>, cors(Req)),
            {ok, Req4, Env};
        Imei ->
            Skey = base64:encode(Imei),

            System = case Method of
                <<"OPTIONS">> -> undefined;
                _ ->
                    navidb:get(system, Skey, cached)
                end,

            State = #{
                skey => Skey,
                imei => Imei,
                system => System,
                params => maps:merge(Params, Query),
                method => Method,
                dynamic => DynamicInit
            },

            Req4 =
            try handle(Method, Handler, Req, State) of
                #{nocommands := true, response := RespBody} ->
                    navidb:set(dynamic, Skey, DynamicInit),
                    cowboy_req:reply(200, [
                        {<<"content-type">>, <<"application/octet-stream">>}
                    ], RespBody, cors(Req));
                #{dynamic := Dynamic, response := RespBody} ->
                    navidb:set(dynamic, Skey, maps:merge(DynamicInit, Dynamic)),
                    cowboy_req:reply(200, [
                        {<<"content-type">>, <<"application/octet-stream">>}
                    ], RespBody, cors(Req));
                #{response := RespBody} ->
                    navidb:set(dynamic, Skey, DynamicInit),
                    cowboy_req:reply(200, [
                        {<<"content-type">>, <<"application/octet-stream">>}
                    ], commands(RespBody, State), cors(Req))
                % {error, ErrorRespBody} ->
                %     cowboy_req:reply(400, [
                %         {<<"content-type">>, <<"application/octet-stream">>}
                %     ], ErrorRespBody, Req)
            catch Class:Reason ->
                StackTrace = erlang:get_stacktrace(),
                io:format("Backtrace = ~p~n", [StackTrace]),
                {Module, Fun, _, _} = hd(StackTrace),
                CriticalRespBody = erlang:list_to_binary(io_lib:format("Internal error: ~p:~p @ ~p:~p~n", [Class, Reason, Module, Fun])),
                lager:error("Error ~p at ~p", [Reason, Class]),
                cowboy_req:reply(500, [
                    {<<"content-type">>, <<"application/octet-stream">>}
                ], CriticalRespBody, Req)
            end,

            folsom_metrics:histogram_timed_notify(Begin),

            {ok, Req4, Env}
    end.

handle(<<"GET">>, Handler, _Req, State) ->
    Handler:get(State);

handle(<<"POST">>, Handler, Req, State) ->
    {ok, Body, _Req1} = cowboy_req:body(Req),
    Handler:post(Body, State);

handle(<<"OPTIONS">>, _Handler, _Req, _State) ->
    #{response => <<"">>}.
    % cowboy_req:reply(200, [], <<"">>, Req).


commands(Body, #{skey := Skey}) ->
    case navidb:get(command, Skey) of
        {ok, Command} ->
            <<Command/binary, Body/binary>>;
        _ ->
            Body
    end.

terminate(_Reason, _Req, _State) ->
    ok.

cors(Req) ->
    cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>,
    cowboy_req:set_resp_header(<<"Access-Control-Allow-Headers">>, <<"Authorization, Content-Type, X-Requested-With, Content-Length">>, Req)).

unixtime() ->
        {A, B, _} = os:timestamp(),
        (A * 1000000) + B.
