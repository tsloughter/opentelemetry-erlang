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
%% @doc A module of functionality shared between OTLP exporters for
%% the various signals.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_exporter_otlp).

-export([init/1,
         export_http/6,
         export_grpc/5,
         endpoints/2]).

-include_lib("kernel/include/logger.hrl").

-type headers() :: [{unicode:chardata(), unicode:chardata()}].
-type endpoint() :: uri_string:uri_string() | uri_string:uri_map() |
                    endpoint_map() | {atom(), unicode:chardata(), integer(), list()}.
-type endpoint_map() :: #{scheme := unicode:chardata(),
                          host := unicode:chardata(),
                          path => unicode:chardata(),
                          port => integer(),
                          ssl_options => list()}.

-type protocol() :: grpc | http_protobuf | http_json.
-type compression() :: gzip.

-type httpc_option() :: {proxy, {{string(), non_neg_integer()}, [string()]}}
                      | {https_proxy, {{string(), non_neg_integer()}, [string()]}}
                      | {max_connections_open, integer() | infinity}
                      | {max_sessions, integer()}
                      | {max_keep_alive_length, integer()}
                      | {keep_alive_timeout, integer()}
                      | {max_pipeline_length, integer()}
                      | {pipeline_timeout, integer()}
                      | {cookies, enabled | disabled | verify}
                      | {ipfamily, inet | inet6 | local | inet6fb4}
                      | {ip, inet:ip_address()}
                      | {port, non_neg_integer()}
                      | {socket_opts, [term()]}
                      | {verbose, false | verbose | debug | trace}
                      | {unix_socket, string()}.

-type ssl_options() :: list() | {system_defaults, list()}.

-type opts() :: #{endpoints := [endpoint()],
                  headers := headers(),
                  protocol := protocol(),
                  compression := compression() | undefined,
                  ssl_options := ssl_options() | undefined,
                  channel_opts => map(),
                  httpc_options => [httpc_option()]}.

-export_type([opts/0,
              headers/0,
              compression/0,
              endpoint_map/0,
              endpoint/0,
              protocol/0]).

-type state() :: #{channel := term() | undefined,
                   httpc_profile := atom() | undefined,
                   protocol := protocol(),
                   channel_pid := pid() | undefined,
                   headers := headers(),
                   compression := compression() | undefined,
                   grpc_metadata := map() | undefined,
                   endpoints := [endpoint_map()]}.

%% @doc Initialize the exporter based on the provided configuration.
-spec init(opts()) -> {ok, state()}.
init(#{endpoints := ConfiguredEndpoints,
       headers := ConfiguredHeaders,
       protocol := Protocol,
       compression := ConfiguredCompression,
       ssl_options := SSLOptions}=Opts) ->
    State = #{channel => undefined,
              httpc_profile => undefined,
              protocol => http_protobuf,
              channel_pid => undefined,
              headers => [],
              compression => undefined,
              grpc_metadata => undefined,
              endpoints => []},

    Headers = headers(ConfiguredHeaders),
    Compression = ConfiguredCompression,
    case Protocol of
        grpc ->
            Endpoints = endpoints(ConfiguredEndpoints, SSLOptions),
            ChannelOpts = maps:get(channel_opts, Opts, #{}),
            UpdatedChannelOpts = case Compression of
                                   undefined -> ChannelOpts;
                                   Encoding -> maps:put(encoding, Encoding, ChannelOpts)
                                 end,

            %% Channel name can be any term. To separate Channels per
            %% the process calling the exporter  use the current pid
            Channel = self(),
            case grpcbox_channel:start_link(Channel,
                                            grpcbox_endpoints(Endpoints),
                                            UpdatedChannelOpts) of
                {ok, ChannelPid} ->
                    {ok, State#{channel => Channel,
                                channel_pid => ChannelPid,
                                endpoints => Endpoints,
                                headers => Headers,
                                compression => Compression,
                                grpc_metadata => headers_to_grpc_metadata(Headers),
                                protocol => grpc}};
                ErrorOrIgnore ->
                    %% TODO: do something different for `already_started' error?

                    %% even if it is `ignore' we should just use `http_protobuf' because
                    %% `ignore' should never happen and means something is wrong
                    ?LOG_WARNING("unable to start grpc channel for exporting and falling back "
                                 "to http_protobuf protocol. reason=~p", [ErrorOrIgnore]),
                    {ok, State#{endpoints => Endpoints,
                                headers => Headers,
                                compression => Compression,
                                protocol => http_protobuf}}
            end;
        http_protobuf ->
            HttpcProfile = start_httpc(Opts),
            Endpoints = endpoints(ConfiguredEndpoints, SSLOptions),
            {ok, State#{httpc_profile => HttpcProfile,
                        endpoints => Endpoints,
                        headers => Headers,
                        compression => Compression,
                        protocol => http_protobuf}};
        http_json ->
            HttpcProfile = start_httpc(Opts),
            Endpoints = endpoints(ConfiguredEndpoints, SSLOptions),
            {ok, State#{httpc_profile => HttpcProfile,
                        endpoints => Endpoints,
                        headers => Headers,
                        compression => Compression,
                        protocol => http_json}}
    end.

%% use a unique httpc profile per exporter
start_httpc(Opts) ->
    HttpcProfile = list_to_atom(lists:concat([?MODULE, "_", erlang:pid_to_list(self())])),

    case httpc:info(HttpcProfile) of
        {error, {not_started, _}} ->
            %% by default use inet6fb4 which will try ipv6 and then fallback to ipv4 if it fails
            HttpcOptions0 = lists:usort(maps:get(httpc_options, Opts, [])),
            HttpcOptions = case lists:keymember(ipfamily, 1, HttpcOptions0) of
                               true -> HttpcOptions0;
                               false -> lists:sort([{ipfamily, inet6fb4} | HttpcOptions0])
                           end,
            %% can't use `stand_alone' because then `httpc:info(Profile)' would fail
            {ok, Pid} = inets:start(httpc, [{profile, HttpcProfile}]),
            ok = httpc:set_options(HttpcOptions, Pid);
        _ ->
            %% profile already started
            ok
    end,
    HttpcProfile.

%% @doc Export OTLP protocol telemery data to the configured endpoints.
export_http(Address, Headers, Body, Compression, SSLOptions, HttpcProfile) ->
    {NewHeaders, NewBody} =
        case Compression of
            gzip -> {[{"content-encoding", "gzip"} | Headers], zlib:gzip(Body)};
            _ -> {Headers, Body}
        end,

    case httpc:request(post, {Address, NewHeaders, "application/x-protobuf", NewBody},
                       [{ssl, SSLOptions}], [], HttpcProfile) of
        {ok, {{_, Code, _}, _, _}} when Code >= 200 andalso Code =< 202 ->
            ok;
        {ok, {{_, Code, _}, _, Message}} ->
            ?LOG_INFO("error response from service exported to status=~p ~s",
                      [Code, Message]),
            error;
        {error, Reason} ->
            ?LOG_INFO("client error exporting ~p", [Reason]),
            error
    end.

export_grpc(GrpcCtx, GrpcServiceModule, Metadata, Request, Channel) ->
    GrpcCtx1 = grpcbox_metadata:append_to_outgoing_ctx(GrpcCtx, Metadata),
    case GrpcServiceModule:export(GrpcCtx1, Request, #{channel => Channel}) of
        {ok, _Response, _ResponseMetadata} ->
            ok;
        {error, {Status, Message}, _} ->
            ?LOG_INFO("OTLP grpc export failed with GRPC status ~s : ~s", [Status, Message]),
            error;
        {http_error, {Status, _}, _} ->
            ?LOG_INFO("OTLP grpc export failed with HTTP status code ~s", [Status]),
            error;
        {error, Reason} ->
            ?LOG_INFO("OTLP grpc export failed with error: ~p", [Reason]),
            error
    end.

grpcbox_endpoints(Endpoints) ->
    [{scheme(Scheme), Host, Port, maps:get(ssl_options, Endpoint, [])} ||
        #{scheme := Scheme, host := Host, port := Port} = Endpoint <- Endpoints].

headers_to_grpc_metadata(Headers) ->
    lists:foldl(fun({X, Y}, Acc) ->
                        maps:put(unicode:characters_to_binary(X), unicode:characters_to_binary(Y), Acc)
                end, #{}, Headers).

%% make all headers into list strings
headers(List) when is_list(List) ->
    Headers =[{unicode:characters_to_list(X), unicode:characters_to_list(Y)} || {X, Y} <- List],
    add_user_agent(Headers);
headers(_) ->
    add_user_agent([]).

add_user_agent(Headers) ->
    case lists:search(fun({Header, _}) -> string:to_lower(Header) == "user-agent" end, Headers) of
        {value, _} -> Headers;
        false -> [{"User-Agent", user_agent()} | Headers]
    end.

user_agent() ->
    {ok, ExporterVsn} = application:get_key(opentelemetry_sdk, vsn),
    lists:flatten(io_lib:format("OTel-OTLP-Exporter-erlang/~s", [ExporterVsn])).

-spec endpoints(endpoint() | [endpoint()], ssl_options() | undefined) -> [endpoint_map()].
endpoints(List, DefaultSSLOpts) when is_list(List) ->
    Endpoints = case io_lib:printable_list(List) of
                    true ->
                        [List];
                    false ->
                        List
                end,

    lists:filtermap(fun(E) -> endpoint(E, DefaultSSLOpts) end, Endpoints);
endpoints(Endpoint, DefaultSSLOpts) ->
    lists:filtermap(fun(E) -> endpoint(E, DefaultSSLOpts) end, [Endpoint]).

endpoint(Endpoint, DefaultSSLOpts) ->
    case parse_endpoint(Endpoint, DefaultSSLOpts) of
        false ->
            ?LOG_WARNING("Failed to parse and ignoring exporter endpoint ~p", [Endpoint]),
            false;
        Parsed ->
            Parsed
    end.

parse_endpoint({Scheme, Host, Port, SSLOptions}, _DefaultSSLOpts) when is_list(SSLOptions) ->
    {true, #{scheme => atom_to_list(Scheme),
             host => unicode:characters_to_list(Host),
             port => Port,
             path => [],
             ssl_options => SSLOptions}};
parse_endpoint({Scheme, Host, Port, _}, DefaultSSLOpts) ->
    HostString = unicode:characters_to_list(Host),
    {true, #{scheme => atom_to_list(Scheme),
             host => HostString,
             port => Port,
             path => [],
             ssl_options => update_ssl_opts(HostString, DefaultSSLOpts)}};
parse_endpoint(Endpoint=#{host := Host,
                          scheme := Scheme,
                          path := Path}, DefaultSSLOpts) ->
    HostString = unicode:characters_to_list(Host),
    %% `merge' keeps the value in the second argument if the key is in both
    %% so to always update the scheme/host/port to charlists we set those
    %% separate from port/ssl_options which should only be added if not already
    %% found in `Endpoint'
    {true, maps:merge(#{port => scheme_port(Scheme),
                        %% we only want to run `tls_certificate_check' if absolutely
                        %% necessary, so wrapping the setup of ssl options here
                        %% in a check that it won't just be overwritten by the value
                        %% from the Endpoint anyway
                        ssl_options => case maps:is_key(ssl_options, Endpoint) of
                                           true ->
                                               [];
                                           false ->
                                               update_ssl_opts(HostString, DefaultSSLOpts)
                                       end},
                      Endpoint#{scheme => to_charlist(Scheme),
                                host => HostString,
                                path => unicode:characters_to_list(Path)})};
parse_endpoint(String, DefaultSSLOpts) when is_list(String) orelse is_binary(String) ->
    case unicode:characters_to_list(String) of
        {_, _, _} ->
            ?LOG_WARNING("error converting endpoint URI ~s to utf8", [String]),
            false;
        UnicodeList ->
            case uri_string:parse(UnicodeList) of
                {error, Reason, Message} ->
                    ?LOG_WARNING("error parsing endpoint URI: ~s : ~p", [Reason, Message]),
                    false;
                ParsedUri ->
                    ParsedUri1 = maybe_add_scheme_port(ParsedUri),
                    parse_endpoint(ParsedUri1, DefaultSSLOpts)
            end
    end;
parse_endpoint(_, _) ->
    false.

to_charlist(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_charlist(Other) ->
    unicode:characters_to_list(Other).

scheme_port(Scheme) when not is_atom(Scheme) ->
    scheme_port(scheme(Scheme));
scheme_port(http) ->
    80;
scheme_port(https) ->
    443;
scheme_port(_) ->
    %% unknown scheme
    80.

maybe_add_scheme_port(Uri=#{port := _Port}) ->
    Uri;
maybe_add_scheme_port(Uri=#{scheme := "http"}) ->
    Uri#{port => 80};
maybe_add_scheme_port(Uri=#{scheme := "https"}) ->
    Uri#{port => 443};
%% an unknown scheme
maybe_add_scheme_port(Uri) ->
    Uri.


%% if no ssl opts are defined by the user then use defaults from `tls_certificate_check'
update_ssl_opts(Host, undefined) ->
    tls_certificate_check:options(Host);
update_ssl_opts(Host, {system_defaults, SSLOptions}) ->
    SSLOptions ++ tls_certificate_check:options(Host);
update_ssl_opts(_, SSLOptions) ->
    SSLOptions.


scheme(Scheme) when Scheme =:= "https" orelse Scheme =:= <<"https">> ->
    https;
scheme(Scheme) when Scheme =:= "http" orelse Scheme =:= <<"http">> ->
    http;
scheme(Scheme) ->
    ?LOG_WARNING("unknown scheme ~p, converting to existing atom, if possible, and using as is", [Scheme]),
    to_existing_atom(Scheme).

to_existing_atom(Term) when is_atom(Term) ->
    Term;
to_existing_atom(Scheme) when is_list(Scheme) ->
    list_to_existing_atom(Scheme);
to_existing_atom(Scheme) when is_binary(Scheme) ->
    %% TODO: switch to binary_to_existing_atom once we drop OTP-22 support
    list_to_existing_atom(binary_to_list(Scheme));
to_existing_atom(_) ->
    erlang:error(bad_exporter_scheme).
