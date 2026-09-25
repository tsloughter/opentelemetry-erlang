%%%------------------------------------------------------------------------
%% Copyright 2021, OpenTelemetry Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc Exports traces using OTLP. Configuration sources, defaults and
%% `OTEL_EXPORTER_OTLP_*' environment variables are normalized by
%% `otel_configuration_sdk' before this exporter is initialized. `init/1'
%% consumes the resulting option map and does not read application or OS
%% environment itself.
%%
%% The normalized option map contains:
%% <ul>
%%   <li>`endpoints': A list of endpoints to send traces to. Can take one of the forms described below. By default, exporter sends data to `http://localhost:4318'.</li>
%%   <li>`headers': List of additional headers to add to export requests.</li>
%%   <li>`protocol': The transport protocol to use, supported values: `grpc' and `http_protobuf'. Defaults to `http_protobuf'.</li>
%%   <li>`compression': Compression to use, supported value: `gzip'. Defaults to no compression.</li>
%%   <li>`ssl_options': a list of SSL options.  See Erlang's <a href='https://www.erlang.org/doc/man/ssl.html#TLS/DTLS%20OPTION%20DESCRIPTIONS%20-%20CLIENT'>SSL docs</a> for what options are available.</li>
%% </ul>
%%
%% Endpoints configuration
%%
%% You can pass your collector endpoints in three forms:
%%
%% <ul>
%%   <li> As a string, i.e `"https://localhost:4000"'.</li>
%%   <li> As a map, with the following keys:
%%     <ul>
%%       <li>`host => unicode:chardata()'</li>
%%       <li>`path => unicode:chardata()'</li>
%%       <li>`port => integer() >= 0 | undefined'</li>
%%       <li>`scheme => unicode:chardata()'</li>
%%     </ul>
%%   </li>
%%   <li> As a 4 element tuple in format `{Scheme, Host, Port, SSLOptions}'.</li>
%% </ul>
%%
%% While using `http_protobuf' protocol, currently only the first endpoint in that list is used to export traces, the rest is effectively ignored. `grpc' supports multiple endpoints.
%%
%% @end
%%%-------------------------------------------------------------------------
-module(otel_exporter_otlp_span).

-behaviour(otel_exporter_span).

-export([init/1,
         export/2,
         shutdown/1]).

-record(state, {protocol :: grpc | http_protobuf,
                transport :: map()}).

%% @doc Initialize the exporter based on the provided configuration.
-spec init(otel_exporter_otlp:opts()) -> {ok, #state{}} | {error, term()}.
init(Opts) ->
    case otel_exporter_otlp:init(Opts, opentelemetry_trace_service) of
        {ok, Transport=#{protocol := Protocol}} ->
            {ok, #state{protocol=Protocol, transport=Transport}};
        {error, _}=Error ->
            Error
    end.

%% @doc Export OTLP protocol telemery data to the configured endpoints.
export(Batch, #state{protocol=http_protobuf, transport=Transport}) ->
    case otel_otlp_traces:to_proto(Batch) of
        empty ->
            ok;
        ProtoMap ->
            Body = opentelemetry_exporter_trace_service_pb:encode_msg(
                     ProtoMap, export_trace_service_request),
            otel_exporter_otlp:export(Body, Transport)
    end;
export(Batch, #state{protocol=grpc, transport=Transport}) ->
    case otel_otlp_traces:to_proto(Batch) of
        empty ->
            ok;
        Request ->
            otel_exporter_otlp:export(Request, Transport)
    end.

%% @doc Shutdown the exporter.
shutdown(#state{transport=Transport}) ->
    otel_exporter_otlp:shutdown(Transport).
