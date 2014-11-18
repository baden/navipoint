%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%% @copyright Denis Batrak
%% @author Batrak Denis <baden.i.ua@gmail.com>
%% @version {@vsn}, {@date} {@time}
%% @doc ErlNaviCC tracker data point
%% @end
%%%-------------------------------------------------------------------
-module(navipoint_app).

-behaviour(application).

-define(APP, navipoint).

%% Application callbacks
-export([start_phase/3, start/2, stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    navipoint_sup:start_link().

stop(_State) ->
    ok.

start_phase(listen, _Type, _Args) ->
    % {ok, Point}  = application:get_env(erlnavicc, point),

    Dispatch = [
        {'_', [
            {"/info", navipoint_info, []},
            % {"/bingps", navipoint_bingps, Point},
            {"/bingps", navipoint_bingps, []},
            {"/config", navipoint_config, []},
            {"/params", navipoint_params, []},
            {"/firmware", navipoint_firmware, []},
            {"/addlog", navipoint_addlog, []}
        ]}
    ],

    cowboy:start_http(?APP, config(acceptors, 100),
                      [{port, config(http_listen_port)}],
                      [{env,
                        [{dispatch, cowboy_router:compile(Dispatch)}]}]),

    % TODO: Метрики
    ok = folsom_metrics:new_counter(point),
    ok = folsom_metrics:new_meter(point_meter),
    ok = folsom_metrics:new_counter(point_error),
    ok = folsom_metrics:new_counter(point_error_crc),
    ok = folsom_metrics:new_counter(point_error_data),
    % ok = folsom_metrics:new_histogram(point_duration), % Статистика примерно по последним 1028 запросам
    % ok = folsom_metrics:new_histogram(point_duration, slide_uniform), % Статистика за последнюю минуту
    ok = folsom_metrics:new_histogram(point_duration, slide_sorted), % Статистика точно по последним 1028 запросам
    % ok = folsom_metrics:new_histogram(point_duration2),
    % ok = folsom_metrics:new_duration(point_duration),
    ok.

config(Key, Default) ->
    case application:get_env(?APP, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

config(Key) ->
    case application:get_env(?APP, Key) of
        undefined -> erlang:error({missing_config, Key});
        {ok, Val} -> Val
    end.