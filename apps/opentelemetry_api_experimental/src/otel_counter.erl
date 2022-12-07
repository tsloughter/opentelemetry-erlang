%%%------------------------------------------------------------------------
%% Copyright 2019, OpenTelemetry Authors
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
%% @doc Counter is a synchronous Instrument which supports non-negative
%% increments.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_counter).

-export([create/4,
         add/3,
         add/4]).

-include("otel_metrics.hrl").
-include_lib("kernel/include/logger.hrl").

-spec create(Meter, Name, ValueType, Opts) -> otel_instrument:t() when
      Meter :: otel_meter:t(),
      Name :: otel_instrument:name(),
      ValueType :: otel_instrument:value_type(),
      Opts :: otel_meter:opts().
create(Meter, Name, ValueType, Opts) ->
    otel_meter:create_counter(Meter, Name, ValueType, Opts).

-spec add(otel_meter:t(), otel_instrument:name(), number(), opentelemetry:attributes_map()) -> ok.
add(Meter, Name, Number, Attributes) when Number >= 0 ->
    otel_meter:record(Meter, Name, Number, Attributes);
add(_, Name, _, _) ->
    ?LOG_INFO("Counter instrument ~p does not support negative values.", [Name]),
    ok.

-spec add(otel_instrument:t(), number(), opentelemetry:attributes_map()) -> ok.
add(Instrument=#instrument{module=Module,
                           value_type=?VALUE_TYPE_INTEGER}, Number, Attributes)
  when is_integer(Number) andalso Number >= 0 ->
    Module:record(Instrument, Number, Attributes);
add(Instrument=#instrument{module=Module,
                           value_type=?VALUE_TYPE_FLOAT}, Number, Attributes)
  when is_float(Number) andalso Number >= 0 ->
    Module:record(Instrument, Number, Attributes);
add(#instrument{name=Name,
                value_type=?VALUE_TYPE_INTEGER}, Number, _) ->
    ?LOG_DEBUG("Counter instrument ~p does not support adding value ~p. "
               "The value must be a positive integer.", [Name, Number]),
    ok;
add(#instrument{name=Name,
                value_type=?VALUE_TYPE_FLOAT}, Number, _) ->
    ?LOG_DEBUG("Counter instrument ~p does not support adding value ~p. "
               "The value must be a positive float.", [Name, Number]),
    ok.
