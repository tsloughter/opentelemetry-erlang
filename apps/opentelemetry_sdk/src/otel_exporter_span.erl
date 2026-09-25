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
%% @doc Behaviour and lifecycle helpers for span exporters.
%% @end
%%%-----------------------------------------------------------------------
-module(otel_exporter_span).

-export([init/1,
         export/2,
         shutdown/1]).

-type result() :: ok |
                  success |
                  failed_not_retryable |
                  failed_retryable |
                  error |
                  {error, term()}.

-export_type([result/0]).

%% Do any initialization of the exporter here and return state that will be
%% passed along with each span batch to `export/2'.
-callback init(term()) -> {ok, term()} | {error, term()} | ignore.

%% Export a batch of finished spans. Calls for one exporter instance are
%% serialized by the built-in span processors.
-callback export(otel_batch_span:t(), term()) -> result().

-callback shutdown(term()) -> ok.

init(Opts) ->
    otel_exporter_lifecycle:init(Opts).

export({ExporterModule, State}, Batch) ->
    ExporterModule:export(Batch, State).

shutdown(Exporter) ->
    otel_exporter_lifecycle:shutdown(Exporter).
