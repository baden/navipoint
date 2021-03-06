%% -*- coding: utf-8 -*-
-module(navipoint_config).
-behaviour(navipoint_sub_protocol).

-export([init/2, get/1, post/2]).

-spec init(any(), any()) -> {atom(), any(), any()}.
init(Req, Opts) ->
    {navipoint_handler, Req, Opts}.

-spec get(map:map()) -> map:map().
get(_Query) ->
    notfound.

-spec post(binary(), map:map()) -> map:map().
post(Body, #{skey := Skey, params := Query}) ->
    Lines = string:tokens(binary_to_list(Body), "\r\n"),
    Parced = lists:foldl(
        fun(Line, Acc) ->
            case string:tokens(Line, " \t") of
                ["END"] ->
                    Acc;
                [KEY, TYPE, VALUE, DEFAULT] ->
                    Key = list_to_binary(KEY),
                    Value = binary:replace(list_to_binary(VALUE), <<"\"">>, <<"">>, [global]),
                    Default = binary:replace(list_to_binary(DEFAULT), <<"\"">>, <<"">>, [global]),

                    Element = #{
                        type    => list_to_binary(TYPE),
                        value   => Value,
                        default => Default
                    },
                    maps:put(Key, Element, Acc)
            end
        end,
        #{},
        Lines
    ),

    navidb:set(params, Skey, #{data => Parced}),

    case maps:get(<<"phone">>, Query, undefined) of
        undefined -> ok;
        Phone ->
            navidb:set(systems, Skey, #{phone => Phone})
    end,

    #{response => <<"CONFIG: OK\r\n">>}.
