-module(otel_configuration_keys_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").

all() -> [native_unknown_keys_fail, json_unknown_keys_warn,
          native_tuples_and_named_providers, legacy_resource_shape,
          custom_properties_are_opaque].

native_unknown_keys_fail(_Config) ->
    lists:foreach(
      fun({Configuration, Path}) ->
              ?assertEqual({error, {invalid_configuration, Path, unknown_property}},
                           resolve_native(Configuration)),
              ?assertEqual({error, {invalid_configuration, binary_leaf(Path), unknown_property}},
                           resolve_native(json_keys(Configuration)))
      end, cases()).

json_unknown_keys_warn(_Config) ->
    Handler = unknown_key_test,
    ok = logger:add_handler(Handler, ?MODULE,
                            #{level => warning, config => #{pid => self()}}),
    try
        lists:foreach(
          fun({Configuration, Path}) ->
                  Json = (json_keys(Configuration))#{<<"file_format">> => <<"1.1">>},
                  {ok, Runtime} = otel_configuration_declarative:resolve(Json),
                  ?assertEqual(Json, otel_configuration_model:root(otel_configuration_sdk:source(Runtime))),
                  ?assertEqual([binary_leaf(Path)], warnings())
          end, cases()),
        Unknown = <<"unknown_native_option_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
        ?assertException(error, badarg, binary_to_existing_atom(Unknown, utf8)),
        ?assertEqual({error, {invalid_configuration, [Unknown], unknown_property}},
                     resolve_native(#{Unknown => <<"sensitive value">>})),
        ?assertMatch({ok, _}, otel_configuration_declarative:resolve(
                               #{<<"file_format">> => <<"1.1">>, Unknown => <<"sensitive value">>})),
        ?assertEqual([[Unknown]], warnings()),
        ?assertException(error, badarg, binary_to_existing_atom(Unknown, utf8))
    after
        logger:remove_handler(Handler)
    end.

native_tuples_and_named_providers(_Config) ->
    lists:foreach(
      fun(Processor) ->
              Provider = #{processors => [{Processor, #{exporter =>
                                  {otlp_http, #{endpoints => [<<"https://collector/v1/traces">>]}}}}]},
              Error = {error, {invalid_configuration, [exporter, otlp_http, endpoints], unknown_property}},
              ?assertEqual(Error, otel_configuration_sdk:create_tracer_provider(Provider)),
              ?assertEqual(Error, otel_tracer_provider_sup:start(invalid_options_provider, Provider)),
              ?assertEqual(undefined, whereis(otel_tracer_provider_invalid_options_provider))
      end, [batch, simple, otel_batch_processor, otel_simple_processor]),
    ?assertEqual({error, {invalid_configuration,
                         [tracer_provider, sampler, parent_based, remote_parent_smpled], unknown_property}},
                 otel_configuration_sdk:create_tracer_provider(
                   #{processors => [], sampler => {parent_based, #{remote_parent_smpled => always_off}}})),
    ?assertMatch({ok, #{sampler := {parent_based, #{root := always_off}}}},
                 otel_configuration_sdk:create_tracer_provider(
                   #{processors => [], sampler => {parent_based, #{<<"root">> => always_off}}})).

legacy_resource_shape(_Config) ->
    lists:foreach(
      fun(Attributes) ->
              ?assertEqual({error, {invalid_configuration, [resource], legacy_configuration_not_supported}},
                           resolve_native(#{resource => Attributes}))
      end, [#{<<"service.name">> => <<"x">>}, #{'service.name' => <<"x">>}]),
    ?assertMatch({ok, _}, resolve_native(#{resource => #{attributes => #{<<"service.name">> => <<"x">>}}})),
    ?assertMatch({ok, _}, resolve_native(#{resource => #{}})).

custom_properties_are_opaque(_Config) ->
    Options = #{endpoints => [], remote_parent_smpled => custom_value, arbitrary => #{nested => true}},
    {ok, Runtime} = resolve_native(
        #{resource => #{attributes => #{<<"tracer_providers">> => <<"attribute value">>}},
          propagator => #{composite => [{custom_propagator, Options}]},
          tracer_provider =>
              #{processors => [{custom_processor, Options},
                               {batch, #{exporter => {custom_exporter, Options}}}],
                sampler => {custom_sampler, Options}, id_generator => custom_generator},
          meter_provider => #{future_reader_setting => Options},
          logger_provider => #{future_logger_setting => Options}}),
    ?assertMatch(#{processors := [{custom_processor, Options},
                                  {otel_batch_processor, #{exporter := {custom_exporter, Options}}}],
                   sampler := {custom_sampler, Options}, id_generator := custom_generator},
                 otel_configuration_sdk:tracer_provider(Runtime)),
    ?assertEqual([{custom_propagator, Options}], otel_configuration_sdk:text_map_propagators(Runtime)),
    %% Transport-library options in the explicit module form remain opaque.
    ?assertMatch({ok, _}, resolve_native(exporter(opentelemetry_exporter,
                              #{channel_opts => Options, httpc_options => [{custom_option, true}]}))).

cases() ->
    [{#{tracer_providers => #{}}, [tracer_providers]},
     {#{resource => #{atributes => #{}}}, [resource, atributes]},
     {#{resource => #{attributes => [#{name => <<"a">>, value => <<"b">>, valeu => 1}]}},
      [resource, attributes, valeu]},
     {#{propagator => #{composit => []}}, [propagator, composit]},
     {#{propagator => #{composite => [#{tracecontext => #{typo => true}}]}},
      [propagator, composite, tracecontext, typo]},
     {#{attribute_limits => #{attribute_counts_limit => 4}}, [attribute_limits, attribute_counts_limit]},
     {provider(#{processor => []}), [tracer_provider, processor]},
     {provider(#{limits => #{attribute_per_event_limit => 4}}), [tracer_provider, limits, attribute_per_event_limit]},
     {provider(#{processors => [#{batch => #{scheduled_delay_ms => 1, exporter => #{console => null}}}]}),
      [tracer_provider, processors, batch, scheduled_delay_ms]},
     {provider(#{processors => [#{simple => #{exporting_timeout_ms => 1, exporter => #{console => null}}}]}),
      [tracer_provider, processors, simple, exporting_timeout_ms]},
     {exporter(otlp_http, #{endpoints => [<<"https://collector/v1/traces">>]}), [exporter, otlp_http, endpoints]},
     {exporter(otlp_grpc, #{ssl_options => []}), [exporter, otlp_grpc, ssl_options]},
     {exporter(otlp_http, #{tls => #{ca_files => <<"ca.pem">>}}), [exporter, otlp_http, tls, ca_files]},
     {exporter(otlp_grpc, #{tls => #{ca_files => <<"ca.pem">>}}), [exporter, otlp_grpc, tls, ca_files]},
     {exporter(otlp_http, #{headers => [#{name => <<"a">>, value => <<"b">>, valeu => 1}]}),
      [exporter, otlp_http, headers, valeu]},
     {exporter(console, #{typo => true}), [exporter, console, typo]},
     {provider(#{sampler => #{parent_based => #{remote_parent_smpled => #{always_on => null}}}}),
      [tracer_provider, sampler, parent_based, remote_parent_smpled]},
     {provider(#{sampler => #{parent_based => #{root => #{trace_id_ratio_based => #{ration => 0.5}}}}}),
      [tracer_provider, sampler, parent_based, root, trace_id_ratio_based, ration]},
     {provider(#{sampler => #{always_on => #{typo => true}}}), [tracer_provider, sampler, always_on, typo]},
     {provider(#{id_generator => #{random => #{typo => true}}}), [tracer_provider, id_generator, random, typo]},
     {#{distribution => #{erlagn => #{}}}, [distribution, erlagn]},
     {#{distribution => #{erlang => #{resource_detector_timeot => 100}}}, [distribution, erlang, resource_detector_timeot]},
     {#{distribution => #{erlang => #{sweeper => #{span_ttl_ms => 100}}}}, [distribution, erlang, sweeper, span_ttl_ms]}].

provider(Options) -> #{tracer_provider => maps:merge(#{processors => []}, Options)}.
exporter(Kind, Options) -> provider(#{processors => [#{batch => #{exporter => #{Kind => Options}}}]}).

resolve_native(Configuration) ->
    {ok, Model} = otel_configuration_model:from_application_env(maps:to_list(Configuration)),
    otel_configuration_sdk:create(Model).

json_keys(Map) when is_map(Map) ->
    maps:from_list([{json_key(Key), json_keys(Value)} || {Key, Value} <- maps:to_list(Map)]);
json_keys(List) when is_list(List) -> [json_keys(Value) || Value <- List];
json_keys(Value) -> Value.

json_key(Key) when is_atom(Key) -> atom_to_binary(Key, utf8);
json_key(Key) -> Key.

binary_leaf(Path) -> lists:droplast(Path) ++ [json_key(lists:last(Path))].

log(#{meta := #{pid := Pid, otel_configuration_path := Path}}, #{config := #{pid := Pid}}) ->
    Pid ! {unknown_key_warning, Path},
    ok;
log(_, _) -> ok.

warnings() ->
    receive {unknown_key_warning, Path} -> [Path | warnings()]
    after 0 -> []
    end.
