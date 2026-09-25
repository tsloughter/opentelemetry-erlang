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

-export([init/2,
         export/2,
         shutdown/1,
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

-type state() :: #{protocol := protocol(),
                   transport := {module(), term()}}.

%% @doc Initialize the exporter based on the provided configuration.
-spec init(opts(), module()) -> {ok, state()} | {error, term()}.
init(#{endpoints := ConfiguredEndpoints,
       headers := ConfiguredHeaders,
       protocol := Protocol,
       ssl_options := SSLOptions}=Opts, GrpcServiceModule) ->
    case initialize_endpoints(ConfiguredEndpoints, SSLOptions) of
        {ok, []} ->
            {error, no_endpoints};
        {ok, Endpoints} ->
            TransportOpts = Opts#{endpoints => Endpoints,
                                  headers => headers(ConfiguredHeaders),
                                  grpc_service => GrpcServiceModule},
            init_transport(Protocol, TransportOpts);
        {error, _}=Error ->
            Error
    end;
init(Opts, _GrpcServiceModule) ->
    {error, {invalid_options, Opts}}.

init_transport(grpc, Opts) ->
    init_transport(grpc, otel_transport_otlp_grpc, Opts);
init_transport(http_protobuf, Opts) ->
    init_transport(http_protobuf, otel_transport_otlp_http, Opts);
init_transport(Protocol, _Opts) ->
    {error, {unsupported_protocol, Protocol}}.

init_transport(Protocol, Module, Opts) ->
    case Module:init(Opts) of
        {ok, TransportState} ->
            {ok, #{protocol => Protocol,
                   transport => {Module, TransportState}}};
        {error, Reason} ->
            {error, {transport_initialization_failed, Module, Reason}};
        ignore ->
            {error, {transport_initialization_failed, Module, ignore}};
        Other ->
            {error, {invalid_transport_init_result, Module, Other}}
    end.

-spec export(term(), state()) -> ok | error | {error, term()}.
export(Payload, #{transport := {Module, TransportState}}) ->
    Module:export(Payload, TransportState).

-spec shutdown(state()) -> ok.
shutdown(#{transport := {Module, TransportState}}) ->
    Module:shutdown(TransportState).

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
    lists:filtermap(fun(E) -> endpoint(E, DefaultSSLOpts) end,
                    endpoint_values(List));
endpoints(Endpoint, DefaultSSLOpts) ->
    lists:filtermap(fun(E) -> endpoint(E, DefaultSSLOpts) end, [Endpoint]).

initialize_endpoints(Endpoints, DefaultSSLOpts) ->
    initialize_endpoints(endpoint_values(Endpoints), DefaultSSLOpts, []).

initialize_endpoints([], _DefaultSSLOpts, Acc) ->
    {ok, lists:reverse(Acc)};
initialize_endpoints([Endpoint | Rest], DefaultSSLOpts, Acc) ->
    case parse_endpoint(Endpoint, DefaultSSLOpts) of
        {true, Parsed} ->
            initialize_endpoints(Rest, DefaultSSLOpts, [Parsed | Acc]);
        false ->
            {error, {invalid_endpoint, Endpoint}}
    end.

endpoint_values([]) ->
    [];
endpoint_values(List) when is_list(List) ->
    case io_lib:printable_list(List) of
        true -> [List];
        false -> List
    end;
endpoint_values(Endpoint) ->
    [Endpoint].

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

scheme_port(Scheme) when Scheme =:= http; Scheme =:= "http"; Scheme =:= <<"http">> ->
    80;
scheme_port(Scheme) when Scheme =:= https; Scheme =:= "https"; Scheme =:= <<"https">> ->
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
