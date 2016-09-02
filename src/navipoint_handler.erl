%% -*- coding: utf-8 -*-
-module(navipoint_handler).

% -behaviour(navipoint_sub_protocol).
-behaviour(cowboy_sub_protocol).

-export([
        upgrade/6
    ]).

-export([terminate/3]).

-callback init(any(), any()) -> {atom(), any(), any()}.
-callback get(map:map()) -> map:map().
-callback post(binary(), map:map()) -> map:map().

% -spec upgrade(_Req, _Env, _Handler, _HandlerState, infinity, run) -> {ok, any(), any()}.
upgrade(Req, Env, Handler, _HandlerState, infinity, run) ->
    navistats:notify(point, {inc, 1}),
    navistats:notify(point_meter, 1),
    Begin = navistats:histogram_timed_begin(point_duration),

    % Start = now(),
    Method = cowboy_req:method(Req),
    % Headers = maps:from_list(cowboy_req:headers(Req)),
    Params = maps:from_list(cowboy_req:bindings(Req)),
    Query  = maps:from_list(cowboy_req:parse_qs(Req)),

    Csq  = int_value(csq, Query),
    Vin  = int_value(vin, Query, -1),
    Vout = int_value(vout, Query, -1),

    DynamicInit = #{
        lastping => unixtime(),
        method   => Method,
        csq      => Csq,
        vin      => Vin / 1000,
        vout     => Vout / 1000
    },

    % Все входящие сообщения должны содержать параметр imei. Иначе отбой соединения
    case maps:get(<<"imei">>, Query, undefined) of
        undefined ->
            Req4 = cowboy_req:reply(
              500,
              [{<<"content-type">>, <<"application/octet-stream">>}],
              <<"ERROR: imei query must be defined">>,
              cors(Req)
            ),
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
                notfound ->
                    cowboy_req:reply(404, [
                        {<<"content-type">>, <<"application/octet-stream">>}
                    ], <<"NOT_FOUND\r\n">>, Req);
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
                    % navipoint_handler.erl:87:
                    % The call
                    % navipoint_handler:commands(
                    %   RespBody::any(),
                    %   State::#{
                    %       'dynamic':=#{
                    %           'csq':=integer(),
                    %           'lastping':=non_neg_integer(),
                    %           'method':=binary(),
                    %           'vin':=float(),
                    %           'vout':=float()
                    %       },
                    %       'imei':=binary() | [1..255],
                    %       'method':=binary(),
                    %       'params':=#{
                    %           atom() | binary()=>'true' | binary()
                    %       },
                    %       'skey':=binary(),
                    %       'system':=_
                    %   }
                    % )
                    % will never return since it differs in the 2nd argument from the success typing arguments:
                    % (any(),#{'skey':=[binary()], _=>_})

                    BodyPlusCommands = commands(RespBody, State),
                    cowboy_req:reply(200, [
                        {<<"content-type">>, <<"application/octet-stream">>}
                    ], BodyPlusCommands, cors(Req))
                % {error, ErrorRespBody} ->
                %     cowboy_req:reply(400, [
                %         {<<"content-type">>, <<"application/octet-stream">>}
                %     ], ErrorRespBody, Req)
            catch Class:Reason ->
                StackTrace = erlang:get_stacktrace(),
                io:format("Backtrace = ~p~n", [StackTrace]),
                {Module, Fun, _, _} = hd(StackTrace),
                CriticalRespBody = critical_body(Class, Reason, Module, Fun),
                lager:error("Error ~p at ~p", [Reason, Class]),
                cowboy_req:reply(500, [
                    {<<"content-type">>, <<"application/octet-stream">>}
                ], CriticalRespBody, Req)
            end,

            navistats:histogram_timed_notify(Begin),

            {ok, Req4, Env}
    end.

critical_body(Class, Reason, Module, Fun) ->
    erlang:list_to_binary(io_lib:format("Internal error: ~p:~p @ ~p:~p~n", [Class, Reason, Module, Fun])).

handle(<<"GET">>, Handler, _Req, State) ->
    Handler:get(State);

handle(<<"POST">>, Handler, Req, State) ->
    {ok, Body, _Req1} = cowboy_req:body(Req),
    Handler:post(Body, State);

handle(<<"OPTIONS">>, _Handler, _Req, _State) ->
    #{response => <<"">>}.
    % cowboy_req:reply(200, [], <<"">>, Req).


% -spec commands(binary(), map:map()) -> binary().
-spec commands(binary(), #{'skey':=[binary()], _=>_}) -> binary().
commands(Body, #{skey := Skey}) ->
    case navidb:get(command, Skey) of
        {ok, Command} ->
            <<Command/binary, Body/binary>>;
        _ ->
            Body
    end.

% commands(Body, _) -> Body.

-spec terminate(_, _, _) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.

cors(Req) ->
    cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>,
        cowboy_req:set_resp_header(
            <<"Access-Control-Allow-Headers">>,
            <<"Authorization, Content-Type, X-Requested-With, Content-Length">>, Req
        )
    ).

unixtime() ->
        {A, B, _} = os:timestamp(),
        (A * 1000000) + B.

int_value(Key, Query) ->
    int_value(Key, Query, 0).

int_value(Key, Query, Default) when is_atom(Key) ->
    int_value(atom_to_binary(Key, latin1), Query, Default);

int_value(Key, Query, Default) ->
    try
        erlang:binary_to_integer(maps:get(Key, Query))
    catch
        _:_ -> Default
    end.
