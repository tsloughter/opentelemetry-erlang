%%%------------------------------------------------------------------------
%% Copyright 2026, OpenTelemetry Authors
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
%% @private
%%%-----------------------------------------------------------------------
-module(otel_transport_otlp_grpc).

-behaviour(otel_transport).

-export([init/1,
         export/2,
         shutdown/1]).

-include_lib("kernel/include/logger.hrl").

-record(state, {channel :: term(),
                channel_pid :: pid(),
                metadata :: map(),
                service :: module()}).

init(#{endpoints := Endpoints,
       headers := Headers,
       compression := Compression,
       grpc_service := Service}=Opts) ->
    Channel = maps:get(channel, Opts, make_ref()),
    ChannelOpts0 = maps:get(channel_opts, Opts, #{}),
    ChannelOpts = case Compression of
                      undefined -> ChannelOpts0;
                      Encoding -> ChannelOpts0#{encoding => Encoding}
                  end,
    start_channel(Channel, Endpoints, ChannelOpts, Headers, Service);
init(Opts) ->
    {error, {invalid_options, Opts}}.

start_channel(Channel, Endpoints, ChannelOpts, Headers, Service) ->
    try grpcbox_channel:start_link(Channel,
                                   grpcbox_endpoints(Endpoints),
                                   ChannelOpts) of
        {ok, Pid} ->
            {ok, #state{channel=Channel,
                        channel_pid=Pid,
                        metadata=headers_to_metadata(Headers),
                        service=Service}};
        {error, Reason} ->
            {error, {grpc_channel_start_failed, Reason}};
        ignore ->
            {error, grpc_channel_start_ignored}
    catch
        Class:Reason ->
            {error, {grpc_channel_start_failed, {Class, Reason}}}
    end.

export(Request, #state{channel=Channel,
                       metadata=Metadata,
                       service=Service}) ->
    GrpcCtx = grpcbox_metadata:append_to_outgoing_ctx(ctx:new(), Metadata),
    case Service:export(GrpcCtx, Request, #{channel => Channel}) of
        {ok, _Response, _ResponseMetadata} ->
            ok;
        {error, {Status, Message}, _} ->
            ?LOG_INFO("OTLP gRPC export failed with status ~s: ~s",
                      [Status, Message]),
            error;
        {http_error, {Status, _}, _} ->
            ?LOG_INFO("OTLP gRPC export failed with HTTP status ~s", [Status]),
            error;
        {error, Reason} ->
            ?LOG_INFO("OTLP gRPC export failed: ~p", [Reason]),
            error
    end.

shutdown(#state{channel=Channel, channel_pid=Pid}) ->
    erlang:unlink(Pid),
    _ = grpcbox_channel:stop(Channel),
    ok.

grpcbox_endpoints(Endpoints) ->
    [{scheme(Scheme), Host, Port, maps:get(ssl_options, Endpoint, [])} ||
        #{scheme := Scheme, host := Host, port := Port} = Endpoint <- Endpoints].

headers_to_metadata(Headers) ->
    maps:from_list([{unicode:characters_to_binary(Name),
                     unicode:characters_to_binary(Value)}
                    || {Name, Value} <- Headers]).

scheme(Scheme) when Scheme =:= "https"; Scheme =:= <<"https">>; Scheme =:= https ->
    https;
scheme(Scheme) when Scheme =:= "http"; Scheme =:= <<"http">>; Scheme =:= http ->
    http;
scheme(Scheme) ->
    erlang:error({unsupported_endpoint_scheme, Scheme}).
