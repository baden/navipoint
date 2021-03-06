%% -*- coding: utf-8 -*-
-module(navipoint_info).

-export([init/2]).
-export([options/2]).
-export([content_types_provided/2]).
-export([hello_to_html/2]).
-export([hello_to_json/2]).

% -include("types.hrl").

-spec init(Req, State) -> {atom(), Req, State}.
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

-spec options(any(), any()) -> {ok, any(), any()}.
options(Req, State) ->
    Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"GET, OPTIONS">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req1),
    {ok, Req2, State}.

-spec content_types_provided(any(), any()) -> {proplists:proplists(), any(), any()}.
content_types_provided(Req, State) ->
    {[
        {<<"text/html">>, hello_to_html},
        {<<"application/json">>, hello_to_json}
    ], Req, State}.

-spec hello_to_html(any(), any()) -> {binary(), any(), any()}.
hello_to_html(Req, State) ->
    Durations = list_to_binary([
        io_lib:format("['1', ~p],", [I]) ||
        I <- navistats:get_metric_value(point_duration)]),

    Histogram = navistats:get_histogram_statistics(point_duration),
    HData = proplists:get_value(histogram, Histogram),
    RData = list_to_binary([
        io_lib:format("[~p, ~p],", [K, V]) ||
        {K, V} <- HData]),

    Table =
        <<"<table><tbody>",
        (metric(point,             navistats:get_metric_value(point)))/binary,
        (metric(point_meter,       navistats:get_metric_value(point_meter)))/binary,
        (metric(point_error,       navistats:get_metric_value(point_error)))/binary,
        (metric(point_error,       navistats:get_metric_value(point_error_crc)))/binary,
        (metric(point_error_data,  navistats:get_metric_value(point_error_data)))/binary,
        % (metric(point_duration,    navistats:get_metric_value(point_duration)))/binary,
        (metric(point_duration_stat, Histogram))/binary,
        "</tbody></table>">>,

    Body = <<
        "<html>\n",
            "<head>\n",
                "<meta charset=\"utf-8\">\n",
                "<title>Point info (3)</title>\n",
                "<script type=\"text/javascript\" src=\"https://www.google.com/jsapi\"></script>\n",
                "<script type=\"text/javascript\">\n",
                "      google.load(\"visualization\", \"1\", {packages:[\"corechart\"]});\n",
                "      google.setOnLoadCallback(drawChart);\n",
                "      function drawChart() {\n",
                "        var data = google.visualization.arrayToDataTable([\n",
                "          ['Requests', 'uSec'],\n",
                Durations/binary,
                "        ]);\n",
                "        var options = {\n",
                "          title: 'Request process time',\n",
                % "           legend: 'none',\n",
                % "                  vAxis: {title: 'Requests',  titleTextStyle: {color: 'red'}, maxValue: 250, viewWindow: {min: 0, max: 766}}\n",
                "                  hAxis: {title: 'uSec',  titleTextStyle: {color: 'red'}}\n",
                "        };\n",
                "        var chart = new google.visualization.LineChart(document.getElementById('chart_div'));\n",
                "        chart.draw(data, options);\n",
                "        data = google.visualization.arrayToDataTable([\n",
                "          ['Requests', 'uSec'],\n",
                % Durations/binary,
                RData/binary,
                "        ]);\n",
                "        options = {\n",
                "                  title: 'Request time statistics',\n",
                "                  vAxis: {title: 'Requests',  titleTextStyle: {color: 'red'}, maxValue: 250, viewWindow: {max: 600}},\n",
                "                  hAxis: {title: 'uSec',  titleTextStyle: {color: 'red'}}\n",
                "                };\n",
                % "        chart = new google.visualization.Histogram(document.getElementById('chart2_div'));\n",
                "        chart = new google.visualization.LineChart(document.getElementById('chart2_div'));\n",
                "        chart.draw(data, options);\n",
                "      }\n",
                "    </script>\n",
            "</head>\n",
            "<body>\n",
                "<p>REST methods: GET</p>\n",
                "<div id=\"chart_div\" style=\"width: 900px; height: 500px;\"></div>\n",
                "<div id=\"chart2_div\" style=\"width: 900px; height: 1000px;\"></div>\n",
                "<p>Metrics:", Table/binary, "</p>\n",
            "</body>\n",
        "</html>\n"
    >>,
    {Body, Req, State}.

metric(Name, Value) ->
    list_to_binary(io_lib:format("<tr><td>~p</td><td><pre>~p</pre></td></tr>", [Name, Value])).

-spec hello_to_json(any(), any()) -> {binary()|string(), any(), any()}.
hello_to_json(Req, State) ->
    Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req),

    % Stats = stats:get(),

    Result = [
        {info, <<"TBD">>}
    ],

    Body = jsx:encode(Result),

    {Body, Req1, State}.
