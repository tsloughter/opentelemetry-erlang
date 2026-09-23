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
%% @doc An export batch of finished spans and their associated resource.
%%
%% The underlying storage is deliberately opaque so span processors can change
%% how batches are represented without changing every span exporter.
%% @end
%%%------------------------------------------------------------------------
-module(otel_batch_span).

-export([new/2,
         resource/1,
         foldl/3,
         foldl_scopes/3]).

-record(batch, {table :: ets:table(),
                resource :: otel_resource:t()}).

-opaque t() :: #batch{}.

-export_type([t/0]).

-spec new(ets:table(), otel_resource:t()) -> t().
new(Table, Resource) ->
    #batch{table=Table, resource=Resource}.

-spec resource(t()) -> otel_resource:t().
resource(#batch{resource=Resource}) ->
    Resource.

-spec foldl(fun((opentelemetry:span(), Acc) -> Acc), Acc, t()) -> Acc.
foldl(Fun, Acc, #batch{table=Table}) ->
    ets:foldl(Fun, Acc, Table).

-spec foldl_scopes(fun((term(), [opentelemetry:span()], Acc) -> Acc), Acc, t()) -> Acc.
foldl_scopes(Fun, Acc, #batch{table=Table}) ->
    foldl_scopes(Fun, Acc, Table, ets:first(Table)).

foldl_scopes(_Fun, Acc, _Table, '$end_of_table') ->
    Acc;
foldl_scopes(Fun, Acc, Table, InstrumentationScope) ->
    Spans = ets:lookup(Table, InstrumentationScope),
    foldl_scopes(Fun,
                 Fun(InstrumentationScope, Spans, Acc),
                 Table,
                 ets:next(Table, InstrumentationScope)).
