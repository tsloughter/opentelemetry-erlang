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
-module(otel_transport_otlp_http).

-behaviour(otel_transport).

-export([init/1,
         export/2,
         shutdown/1]).

-include_lib("kernel/include/logger.hrl").

-record(state, {address :: string(),
                compression :: otel_exporter_otlp:compression() | undefined,
                headers :: otel_exporter_otlp:headers(),
                httpc_pid :: pid(),
                httpc_profile :: atom(),
                ssl_options :: list()}).

init(#{endpoints := [#{scheme := Scheme,
                       host := Host,
                       path := Path,
                       port := Port,
                       ssl_options := SSLOptions} | _],
       headers := Headers,
       compression := Compression}=Opts) ->
    case uri_string:normalize(#{scheme => Scheme,
                                host => Host,
                                port => Port,
                                path => Path}) of
        {error, Type, Error} ->
            {error, {invalid_endpoint, Type, Error}};
        Address ->
            case start_httpc(Opts) of
                {ok, Profile, Pid} ->
                    {ok, #state{address=Address,
                                compression=Compression,
                                headers=Headers,
                                httpc_pid=Pid,
                                httpc_profile=Profile,
                                ssl_options=SSLOptions}};
                {error, _}=Error ->
                    Error
            end
    end;
init(Opts) ->
    {error, {invalid_options, Opts}}.

export(Body, #state{address=Address,
                    compression=Compression,
                    headers=Headers,
                    httpc_profile=HttpcProfile,
                    ssl_options=SSLOptions}) ->
    {RequestHeaders, RequestBody} =
        case Compression of
            gzip -> {[{"content-encoding", "gzip"} | Headers], zlib:gzip(Body)};
            _ -> {Headers, Body}
        end,
    case httpc:request(post,
                       {Address, RequestHeaders,
                        "application/x-protobuf", RequestBody},
                       [{ssl, SSLOptions}], [], HttpcProfile) of
        {ok, {{_, Code, _}, _, _}} when Code >= 200 andalso Code =< 202 ->
            ok;
        {ok, {{_, Code, _}, _, Message}} ->
            ?LOG_INFO("error response from OTLP HTTP service status=~p ~s",
                      [Code, Message]),
            error;
        {error, Reason} ->
            ?LOG_INFO("OTLP HTTP export failed: ~p", [Reason]),
            error
    end.

shutdown(#state{httpc_pid=Pid}) ->
    case inets:stop(httpc, Pid) of
        ok -> ok;
        {error, {not_started, _}} -> ok;
        {error, Reason} ->
            ?LOG_WARNING("unable to stop OTLP HTTP transport: ~p", [Reason]),
            ok
    end.

start_httpc(Opts) ->
    Profile = list_to_atom(lists:concat([?MODULE, "_", erlang:pid_to_list(self()),
                                        "_", erlang:unique_integer([positive])])),
    try inets:start(httpc, [{profile, Profile}]) of
        {ok, Pid} ->
            set_httpc_options(Profile, Pid, httpc_options(Opts));
        {error, Reason} ->
            {error, {httpc_start_failed, Reason}}
    catch
        Class:Reason ->
            {error, {httpc_start_failed, {Class, Reason}}}
    end.

set_httpc_options(Profile, Pid, Options) ->
    try httpc:set_options(Options, Pid) of
        ok ->
            {ok, Profile, Pid};
        {error, Reason} ->
            _ = inets:stop(httpc, Pid),
            {error, {httpc_options_failed, Reason}}
    catch
        Class:Reason ->
            _ = inets:stop(httpc, Pid),
            {error, {httpc_options_failed, {Class, Reason}}}
    end.

httpc_options(Opts) ->
    Options = lists:usort(maps:get(httpc_options, Opts, [])),
    case lists:keymember(ipfamily, 1, Options) of
        true -> Options;
        false -> lists:sort([{ipfamily, inet6fb4} | Options])
    end.
