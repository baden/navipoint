%% -*- coding: utf-8 -*-
-module(navipoint_params).

-export([init/2, get/3]).

init(Req, Opts) ->
    {navipoint_handler, Req, Opts}.

get(Skey, Query, State) ->
    Command = proplists:get_value(<<"cmd">>, Query),
    RespBody = get(Skey, Query, State, Command),
    #{response => RespBody, nocommands => true}.

get(Skey, _Query, _State, <<"params">>) ->
    % Answer = case navidb:get(params, {'_id', Skey}, #{'queue' => 1, '_id' => 0}) of
    Answer = case navidb:get(params, {'_id', Skey}) of
        % #{} ->   % Странно, очередь пуста, возможно нажали отмену
        %     navidb:delete_command(Skey),
        %     <<>>;
        % #{queue := #{}} ->
        %     navidb:delete_command(Skey),
        %     <<>>;
        #{queue := Queue} ->
            lager:warning("----- Queue = ~p", [Queue]),
            maps:fold(
                fun(Key, Value, Acc) ->
                    <<"PARAM ", Key/binary, " ", Value/binary, "\r\n", Acc/binary>>
                end,
                <<>>,
                Queue
            );
        _Other ->
            navidb:delete_command(Skey),
            <<>>
    end,
    <<Answer/binary, "FINISH\r\n">>;

get(Skey, _Query, _State, <<"confirm">>) ->
    % TODO: Не самое оптимальное решение из-за обновления за два этапа
    % case navidb:get(params, {'_id', Skey}, #{'queue' => 1, '_id' => 0}) of
    case navidb:get(params, {'_id', Skey}) of
        % #{} ->   % Странно, очередь пуста, возможно нажали отмену
        %     navidb:set(params, Skey, #{queue => #{}}),
        %     ok;
        % #{<<"queue">> := #{}} ->
        %     navidb:set(params, Skey, {queue, {}}),
        %     ok;
        #{<<"queue">> := Queue} ->
            % Сформируем запрос вида
            % {$set, {'data.PRARAM.value', VALUE}}
            Doc = maps:fold(
                fun(Key, Value, Acc) ->
                    % [<<"data.", (navidb_mongodb:tokey(Key))/binary, ".value">>, Value] ++ Acc
                    maps:put(<<"data.", (navidb_mongodb:tokey(Key))/binary, ".value">>, Value, Acc)
                end,
                #{queue => #{}},
                Queue
            ),
            Update = #{'$set' => Doc},
            navidb:update(params, Skey, Update)
    end,
    % Когда улучшим механизм оповещения, можно будет сделать за одну операцию
    navidb:delete(command, Skey),
    <<"CONFIRM\r\n">>;

get(_Skey, Query, _State, _) ->
    lager:error("Unsupporter cmd command for /params (IMEI=~p)", [proplists:get_value(<<"imei">>, Query)]),
    <<"ERROR: UNSUPPORTED CMD\r\n">>.
