-module(helper).

-include_lib("common_test/include/ct.hrl").

-export([start/1, stop/1, get/3, post/4, random_string/0, crc/1]).

-define(APP, navipoint).
% -define(PORT, 8981).

start(Config) ->
    error_logger:tty(false),

    {ok, Modules} = application:ensure_all_started(?APP),
    {ok, GunModules} = application:ensure_all_started(gun),

    {ok, Port} = application:get_env(?APP, port),

    [{modules, Modules ++ GunModules}, {host, "localhost"}, {port, Port} | Config].

stop(Config) ->
    Modules = ?config(modules, Config),
    [application:stop(Module) || Module <- lists:reverse(Modules)],
    % application:unload(lager), application:unload(navidb), application:unload(naviccapi),
    application:unload(navipoint),
    error_logger:tty(true),
    ok.

get(Config, Url, Params) ->
    Imei = ?config(imei, Config),
    Host = ?config(host, Config),
    Port = ?config(port, Config),

    % Path = io_lib:
    Path = Url ++ "?imei=" ++ url_encode(binary_to_list(Imei)) ++
     lists:flatten(maps:fold(
        fun(Key, Value, Acc) ->
            Element = "&" ++ to_list(Key) ++ "=" ++ url_encode(Value),
            % Element = io_lib:format("&~p=~p", [Key, url_encode(Value)]),
            [Element | Acc]
        end,
        [],
        Params
    )),

    % {ok, ConnPid} = gun:open(Host, Port, #{retry => 0, type => tcp}),
    {ok, ConnPid} = gun:open(Host, Port),
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

to_list(P) when is_atom(P) -> atom_to_list(P);
to_list(P) when is_binary(P) -> binary_to_list(P);
to_list(P) when is_list(P) -> P.

post(Config, Url, Params, Payload) ->
    Imei = ?config(imei, Config),
    Host = ?config(host, Config),
    Port = ?config(port, Config),
    Headers = [
        {<<"content-type">>, <<"application/octet-stream">>}
    ],
    Path = Url ++ "?imei=" ++ url_encode(binary_to_list(Imei)) ++
     lists:flatten(maps:fold(
        fun(Key, Value, Acc) ->
            Element = "&" ++ to_list(Key) ++ "=" ++ url_encode(Value),
            % Element = io_lib:format("&~p=~p", [Key, url_encode(Value)]),
            [Element | Acc]
        end,
        [],
        Params
    )),

    {ok, ConnPid} = gun:open(Host, Port),
    % {ok, ConnPid} = gun:open(Host, Port, [{retry, 0}, {type, tcp}]),
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

-define(CRC16_CCITT_table, [
    16#0000, 16#1021, 16#2042, 16#3063, 16#4084, 16#50a5, 16#60c6, 16#70e7, 16#8108, 16#9129, 16#a14a, 16#b16b,
    16#c18c, 16#d1ad, 16#e1ce, 16#f1ef, 16#1231, 16#0210, 16#3273, 16#2252, 16#52b5, 16#4294, 16#72f7, 16#62d6,
    16#9339, 16#8318, 16#b37b, 16#a35a, 16#d3bd, 16#c39c, 16#f3ff, 16#e3de, 16#2462, 16#3443, 16#0420, 16#1401,
    16#64e6, 16#74c7, 16#44a4, 16#5485, 16#a56a, 16#b54b, 16#8528, 16#9509, 16#e5ee, 16#f5cf, 16#c5ac, 16#d58d,
    16#3653, 16#2672, 16#1611, 16#0630, 16#76d7, 16#66f6, 16#5695, 16#46b4, 16#b75b, 16#a77a, 16#9719, 16#8738,
    16#f7df, 16#e7fe, 16#d79d, 16#c7bc, 16#48c4, 16#58e5, 16#6886, 16#78a7, 16#0840, 16#1861, 16#2802, 16#3823,
    16#c9cc, 16#d9ed, 16#e98e, 16#f9af, 16#8948, 16#9969, 16#a90a, 16#b92b, 16#5af5, 16#4ad4, 16#7ab7, 16#6a96,
    16#1a71, 16#0a50, 16#3a33, 16#2a12, 16#dbfd, 16#cbdc, 16#fbbf, 16#eb9e, 16#9b79, 16#8b58, 16#bb3b, 16#ab1a,
    16#6ca6, 16#7c87, 16#4ce4, 16#5cc5, 16#2c22, 16#3c03, 16#0c60, 16#1c41, 16#edae, 16#fd8f, 16#cdec, 16#ddcd,
    16#ad2a, 16#bd0b, 16#8d68, 16#9d49, 16#7e97, 16#6eb6, 16#5ed5, 16#4ef4, 16#3e13, 16#2e32, 16#1e51, 16#0e70,
    16#ff9f, 16#efbe, 16#dfdd, 16#cffc, 16#bf1b, 16#af3a, 16#9f59, 16#8f78, 16#9188, 16#81a9, 16#b1ca, 16#a1eb,
    16#d10c, 16#c12d, 16#f14e, 16#e16f, 16#1080, 16#00a1, 16#30c2, 16#20e3, 16#5004, 16#4025, 16#7046, 16#6067,
    16#83b9, 16#9398, 16#a3fb, 16#b3da, 16#c33d, 16#d31c, 16#e37f, 16#f35e, 16#02b1, 16#1290, 16#22f3, 16#32d2,
    16#4235, 16#5214, 16#6277, 16#7256, 16#b5ea, 16#a5cb, 16#95a8, 16#8589, 16#f56e, 16#e54f, 16#d52c, 16#c50d,
    16#34e2, 16#24c3, 16#14a0, 16#0481, 16#7466, 16#6447, 16#5424, 16#4405, 16#a7db, 16#b7fa, 16#8799, 16#97b8,
    16#e75f, 16#f77e, 16#c71d, 16#d73c, 16#26d3, 16#36f2, 16#0691, 16#16b0, 16#6657, 16#7676, 16#4615, 16#5634,
    16#d94c, 16#c96d, 16#f90e, 16#e92f, 16#99c8, 16#89e9, 16#b98a, 16#a9ab, 16#5844, 16#4865, 16#7806, 16#6827,
    16#18c0, 16#08e1, 16#3882, 16#28a3, 16#cb7d, 16#db5c, 16#eb3f, 16#fb1e, 16#8bf9, 16#9bd8, 16#abbb, 16#bb9a,
    16#4a75, 16#5a54, 16#6a37, 16#7a16, 16#0af1, 16#1ad0, 16#2ab3, 16#3a92, 16#fd2e, 16#ed0f, 16#dd6c, 16#cd4d,
    16#bdaa, 16#ad8b, 16#9de8, 16#8dc9, 16#7c26, 16#6c07, 16#5c64, 16#4c45, 16#3ca2, 16#2c83, 16#1ce0, 16#0cc1,
    16#ef1f, 16#ff3e, 16#cf5d, 16#df7c, 16#af9b, 16#bfba, 16#8fd9, 16#9ff8, 16#6e17, 16#7e36, 16#4e55, 16#5e74,
    16#2e93, 16#3eb2, 16#0ed1, 16#1ef0
]).

crc(List) ->
   crc(List,16#0000).

crc(<<>>,CRC) ->
   CRC;

crc([],CRC) ->
   CRC;

crc(<<Value:8,Rest/binary>>,CRC) when Value =< 255->
    Index = ((CRC bsr 8) bxor (16#ff band Value)),
    NewCRC = (((CRC bsl 8) band 16#ff00) bxor crc_index(Index)),
    crc(Rest,NewCRC);

crc([Value|Rest],CRC) when Value =< 255->
    Index = ((CRC bsr 8) bxor (16#ff band Value)),
    NewCRC = (((CRC bsl 8) band 16#ff00) bxor crc_index(Index)),
    crc(Rest,NewCRC).

crc_index(N) ->
   lists:nth(N+1,?CRC16_CCITT_table).
