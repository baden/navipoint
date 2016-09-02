%% -*- coding: utf-8 -*-
-module(navipoint_params).

-export([init/2, get/1]).

-spec init(any(), any()) -> {atom(), any(), any()}.
init(Req, Opts) ->
    {navipoint_handler, Req, Opts}.

-spec get(map:map()) -> map:map().
get(#{skey := Skey, params := Params}) ->
    Command = maps:get(<<"cmd">>, Params),
    RespBody = get(Skey, Command),
    #{response => RespBody, nocommands => true}.

get(Skey, <<"params">>) ->
    Answer = case navidb:get(params, {id, Skey}) of
        % #{} ->   % Странно, очередь пуста, возможно нажали отмену
        %     navidb:delete_command(Skey),
        %     <<>>;
        % #{queue := #{}} ->
        %     navidb:delete_command(Skey),
        %     <<>>;
        #{queue := Queue} ->
            maps:fold(
                fun(Key, Value, Acc) ->
                    KeyB = atom_to_binary(Key, latin1),
                    <<"PARAM ", KeyB/binary, " ", Value/binary, "\r\n", Acc/binary>>
                end,
                <<>>,
                Queue
            );
        _Other ->
            navidb:delete(command, Skey),
            <<>>
    end,
    <<Answer/binary, "FINISH\r\n">>;

get(Skey, <<"confirm">>) ->
    % TODO: Не самое оптимальное решение из-за обновления за два этапа
    case navidb:get(params, {id, Skey}) of
        % #{} ->   % Странно, очередь пуста, возможно нажали отмену
        %     navidb:set(params, Skey, #{queue => #{}}),
        %     ok;
        % #{<<"queue">> := #{}} ->
        %     navidb:set(params, Skey, {queue, {}}),
        %     ok;
        #{queue := Queue} ->
            % Сформируем запрос вида
            % {$set, {'data.PRARAM.value', VALUE}}
            % Этот метод теперь не работает
            Doc = maps:fold(
                fun(Key, Value, Acc) ->
                    % [<<"data.", (navidb_mongodb:tokey(Key))/binary, ".value">>, Value] ++ Acc
                    % maps:put(<<"data.", (navidb_mongodb:tokey(Key))/binary, ".value">>, Value, Acc)
                    maps:put([data, Key, value], Value, Acc)
                end,
                #{queue => #{}},
                Queue
            ),
            Update = #{'$set' => Doc},
            navidb:update(params, Skey, Update);
        % No Queue
        _Other ->
            navidb:set(params, Skey, #{queue => #{}}),
            ok
    end,
    % Когда улучшим механизм оповещения, можно будет сделать за одну операцию
    navidb:delete(command, Skey),
    <<"CONFIRM\r\n">>.

% get(_Skey, Query, _State, _) ->
%     lager:error("Unsupporter cmd command for /params (IMEI=~p)", [proplists:get_value(<<"imei">>, Query)]),
%     <<"ERROR: UNSUPPORTED CMD\r\n">>.
