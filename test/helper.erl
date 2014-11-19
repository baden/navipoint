-module(helper).

-export([get/3, post/4, random_string/0]).

-define(PORT, 8981).


get(Imei, Url, Params) ->
    % Path = io_lib:
    Path = Url ++ "?imei=" ++ url_encode(binary_to_list(Imei)) ++
     lists:flatten(maps:fold(
        fun(Key, Value, Acc) ->
            Element = "&" ++ atom_to_list(Key) ++ "=" ++ url_encode(Value),
            % Element = io_lib:format("&~p=~p", [Key, url_encode(Value)]),
            [Element | Acc]
        end,
        [],
        Params
    )),
    ct:pal("Path = ~p", [Path]),

    {ok, ConnPid} = gun:open("localhost", ?PORT, [{retry, 0}, {type, tcp}]),
    Headers = [],
    Ref = gun:get(ConnPid, Path, Headers),
    Response = case gun:await(ConnPid, Ref) of
        {response, nofin, Status, RespHeaders} ->
            {ok, Body} = gun:await_body(ConnPid, Ref),
            {Status, RespHeaders, Body};
        {response, fin, Status, RespHeaders} ->
            {Status, RespHeaders, <<"">>};
        Other ->
            ct:pal("Other = ~p", [Other]),
            Other
    end,
    gun:close(ConnPid),
    Response.

post(Imei, Url, Params, Payload) ->
    Headers = [
        {<<"content-type">>, <<"application/octet-stream">>}
    ],
    Path = Url ++ "?imei=" ++ url_encode(binary_to_list(Imei)) ++
     lists:flatten(maps:fold(
        fun(Key, Value, Acc) ->
            Element = "&" ++ atom_to_list(Key) ++ "=" ++ url_encode(Value),
            % Element = io_lib:format("&~p=~p", [Key, url_encode(Value)]),
            [Element | Acc]
        end,
        [],
        Params
    )),
    ct:pal("Path = ~p", [Path]),

    {ok, ConnPid} = gun:open("localhost", ?PORT, [{retry, 0}, {type, tcp}]),
    Ref = gun:post(ConnPid, Path, Headers, Payload),
    Response = case gun:await(ConnPid, Ref) of
        {response, nofin, Status, RespHeaders} ->
            {ok, Body} = gun:await_body(ConnPid, Ref),
            {Status, RespHeaders, Body};
        {response, fin, Status, RespHeaders} ->
            {Status, RespHeaders, <<"">>};
        Other ->
            ct:pal("Other = ~p", [Other]),
            Other

    end,
    gun:close(ConnPid),
    Response.

url_encode(Data) when is_integer(Data) ->
    url_encode(integer_to_list(Data));

url_encode(Data) when is_binary(Data) ->
    url_encode(binary_to_list(Data));

url_encode(Data) ->
    edoc_lib:escape_uri(Data).

random_string() ->
    base64:encode(crypto:rand_bytes(16)).
