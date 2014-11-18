%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%% @copyright Denis Batrak
%% @author Batrak Denis <baden.i.ua@gmail.com>
%% @version {@vsn}, {@date} {@time}
%% @doc NaviCC GPS data collector
%% @end
%%%-------------------------------------------------------------------
-module(navipoint_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok,
     { {one_for_all, 0, 1},
       [] } }.

%%====================================================================
%% Internal functions
%%====================================================================