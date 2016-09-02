%% -*- coding: utf-8 -*-
-module(navipoint_addlog).

-export([init/2, get/1, post/2]).

init(Req, Opts) ->
    {navipoint_handler, Req, Opts}.

get(#{skey := Skey, params := Params} = _Query) ->
    Text = maps:get(<<"text">>, Params, undefined),
    MType = maps:get(<<"mtype">>, Params, undefined),
    addlog(Skey, MType, Text, Params).

post(Body, #{skey := Skey, params := Params}) ->
    MType = maps:get(<<"mtype">>, Params, undefined),
    % TODO: require inets module
    Text = url_decode(Body),
    addlog(Skey, MType, Text, Params).

addlog(Skey, <<"balance">>, _Text, Params) ->
    PValue = maps:get(<<"value">>, Params, <<"0">>),
    Value =
        try erlang:binary_to_float(PValue)
        catch
            error:badarg ->
                erlang:binary_to_integer(PValue)
        end,
    Dt = unixtime(),

    % navidb_mongodb:insert(?DB_BALANCE, Document),
    Record = #{'value' => Value, 'dt' => Dt},
    navidb:update(systems, Skey, #{'$set' => #{'balance' => Record}}),
    #{response => <<"ADDLOG: OK\r\n">>};

addlog(Skey, _MType, Text, _Params) ->
    Document = #{
        'system' => Skey,
        'dt'     => unixtime(),
        'text'   => Text
    },

    navidb:insert(logs, Document),

    case catchId(Text) of
        {ok, [HWID, SWID]} ->
            % TODO: заменить на set?
            navidb:update(systems, Skey, #{'$set' => #{'hwid' => HWID, 'swid' => SWID}});
        _ -> ok
    end,

    #{response => <<"ADDLOG: OK\r\n">>}.

% post(_Body, _Query) ->
%     #{response => <<"OK\r\n">>}.

catchId(Text) ->
    % Образес сообщения с версией:
    % "Test message. Version line HWID:<b>3081</b> SWID:<b>302E</b>"
    {ok, RE_ID} = re:compile("HWID:<b>([0-9A-Z]+)</b>.*SWID:<b>([0-9A-Z]+)</b>"),

    case re:run(Text, RE_ID, [{capture, [1,2], list}]) of
        {match, [HWID, SWID]} ->
            {ok, [list_to_binary(HWID), list_to_binary(SWID)]};
        _ ->
            {notfound}
    end.

url_decode(Body) ->
    list_to_binary(http_uri:decode(binary_to_list(Body))).

% unixtime() -> timer:now_diff(now(), {0,0,0}) div 1000000.
unixtime() ->
        {A, B, _} = os:timestamp(),
        (A * 1000000) + B.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

url_decode_test() ->
    ?assertEqual(url_decode(<<"123%20456">>), <<"123 456">>).

prepare_function_test() ->
    TextWithId = <<"Test message. Version line HWID:<b>3081</b> SWID:<b>302E</b>">>,
    ?assertEqual(catchId(TextWithId), {ok, [<<"3081">>, <<"302E">>]}),

    TextWithoutId = <<"Just test message.">>,
    ?assertEqual(catchId(TextWithoutId), {notfound}).

-endif.
