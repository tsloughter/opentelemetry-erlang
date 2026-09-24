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
%% @doc Exports logs using OTLP. Configuration sources, defaults and
%% `OTEL_EXPORTER_OTLP_*' environment variables are normalized by
%% `otel_configuration_sdk' before this exporter is initialized. `init/1'
%% consumes the resulting option map and does not read application or OS
%% environment itself.
%%
%% The normalized option map contains:
%% <ul>
%%   <li>`endpoints': A list of endpoints to send logs to. Can take one of the forms described below. By default, exporter sends data to `http://localhost:4318'.</li>
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
%% While using `http_protobuf' protocol, currently only the first endpoint in that list is used to export logs, the rest is effectively ignored. `grpc' supports multiple endpoints.
%%
%% @end
%%%-------------------------------------------------------------------------
-module(otel_exporter_otlp_log).

-behaviour(otel_exporter_log).

-export([init/1,
         export/3,
         shutdown/1]).

-include_lib("kernel/include/logger.hrl").

-record(state, {channel :: term(),
                httpc_profile :: atom() | undefined,
                protocol :: otel_exporter_otlp:protocol(),
                channel_pid :: pid() | undefined,
                headers :: otel_exporter_otlp:headers(),
                compression :: otel_exporter_otlp:compression() | undefined,
                grpc_metadata :: map() | undefined,
                endpoints :: [otel_exporter_otlp:endpoint_map()]}).

%% @doc Initialize the exporter based on the provided configuration.
-spec init(otel_exporter_otlp:opts()) -> {ok, #state{}}.
init(Opts) ->
    case otel_exporter_otlp:init(Opts) of
        {ok, #{channel := Channel,
               channel_pid := ChannelPid,
               endpoints := Endpoints,
               headers := Headers,
               compression := Compression,
               grpc_metadata := Metadata,
               protocol := grpc}} ->
            {ok, #state{channel=Channel,
                        channel_pid=ChannelPid,
                        endpoints=Endpoints,
                        headers=Headers,
                        compression=Compression,
                        grpc_metadata=Metadata,
                        protocol=grpc}};
        {ok, #{httpc_profile := HttpcProfile,
               endpoints := Endpoints,
               headers := Headers,
               compression := Compression,
               protocol := http_protobuf}} ->
            {ok, #state{httpc_profile=HttpcProfile,
                        endpoints=Endpoints,
                        headers=Headers,
                        compression=Compression,
                        protocol=http_protobuf}};
        {ok, #{httpc_profile := HttpcProfile,
               endpoints := Endpoints,
               headers := Headers,
               compression := Compression,
               protocol := http_json}} ->
            {ok, #state{httpc_profile=HttpcProfile,
                        endpoints=Endpoints,
                        headers=Headers,
                        compression=Compression,
                        protocol=http_json}}
    end.

%% @doc Export OTLP protocol telemery data to the configured endpoints.
export(_, _Resource, #state{protocol=http_json}) ->
    {error, unimplemented};
export({Logs, Config}, Resource, #state{protocol=http_protobuf,
                                        httpc_profile=HttpcProfile,
                                        headers=Headers,
                                        compression=Compression,
                                        endpoints=[#{scheme := Scheme,
                                                     host := Host,
                                                     path := Path,
                                                     port := Port,
                                                     ssl_options := SSLOptions} | _]}) ->
    case uri_string:normalize(#{scheme => Scheme,
                                host => Host,
                                port => Port,
                                path => Path}) of
        {error, Type, Error} ->
            ?LOG_INFO("error normalizing OTLP export URI: ~p ~p",
                      [Type, Error]),
            error;
        Address ->
            case otel_otlp_logs:to_proto(Logs, Resource, Config) of
                empty ->
                    ok;
                ProtoMap ->
                    Body = opentelemetry_exporter_logs_service_pb:encode_msg(ProtoMap,
                                                                             export_logs_service_request),
                    otel_exporter_otlp:export_http(Address, Headers, Body, Compression, SSLOptions, HttpcProfile)
            end
    end;
export({Logs, Config}, Resource, #state{protocol=grpc,
                                        grpc_metadata=Metadata,
                                        channel=Channel}) ->
    case otel_otlp_logs:to_proto(Logs, Resource, Config) of
        empty ->
            ok;
        Request ->
            GrpcCtx = ctx:new(),
            otel_exporter_otlp:export_grpc(GrpcCtx, opentelemetry_logs_service, Metadata, Request, Channel)
    end;
export(_, _Resource, _State) ->
    {error, unimplemented}.

%% @doc Shutdown the exporter.
shutdown(#state{channel_pid=undefined}) ->
    ok;
shutdown(#state{channel_pid=Pid}) ->
    _ = grpcbox_channel:stop(Pid),
    ok.
