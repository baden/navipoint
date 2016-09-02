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

-spec start(_, _) -> pid().
start(_StartType, _StartArgs) ->
    navipoint_sup:start_link().

-spec stop(_) -> ok.
stop(_State) ->
    ok.

-spec start_phase(listen, _, _) -> ok.
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
                      [{port, config(port)}],
                      [{env,
                        [{dispatch, cowboy_router:compile(Dispatch)}]}]),

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
