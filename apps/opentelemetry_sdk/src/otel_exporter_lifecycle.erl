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
%% @private
%%%-----------------------------------------------------------------------
-module(otel_exporter_lifecycle).

-export([init/1,
         shutdown/1,
         report_cb/1]).

-include_lib("kernel/include/logger.hrl").

init({ExporterModule, Config}) when is_atom(ExporterModule) ->
    try ExporterModule:init(Config) of
        {ok, ExporterState} ->
            ?LOG_INFO("Exporter ~tp successfully initialized", [ExporterModule]),
            {ExporterModule, ExporterState};
        ignore ->
            undefined
    catch
        Kind:Reason:StackTrace ->
            %% Log the stacktrace at debug level because configuration arguments
            %% in it may contain secrets.
            ?LOG_DEBUG(#{source => exporter,
                         during => init,
                         kind => Kind,
                         reason => Reason,
                         exporter => ExporterModule,
                         stacktrace => StackTrace},
                       #{report_cb => fun ?MODULE:report_cb/1}),
            case {Kind, Reason} of
                {error, undef} ->
                    ?LOG_WARNING("Exporter module ~tp not found. Verify you have included "
                                 "the dependency that contains the exporter module.",
                                 [ExporterModule]);
                _ ->
                    ?LOG_WARNING(#{source => exporter,
                                   during => init,
                                   kind => Kind,
                                   reason => Reason,
                                   exporter => ExporterModule},
                                 #{report_cb => fun ?MODULE:report_cb/1})
            end,
            undefined
    end;
init(Exporter) when Exporter =:= none; Exporter =:= undefined ->
    undefined;
init(ExporterModule) when is_atom(ExporterModule) ->
    init({ExporterModule, []}).

shutdown(undefined) ->
    ok;
shutdown({ExporterModule, State}) ->
    ExporterModule:shutdown(State).

report_cb(#{source := exporter,
            during := init,
            kind := Kind,
            reason := Reason,
            exporter := ExporterModule,
            stacktrace := StackTrace}) ->
    {"Exporter ~tp failed to initialize: ~ts",
     [ExporterModule, otel_utils:format_exception(Kind, Reason, StackTrace)]};
report_cb(#{source := exporter,
            during := init,
            kind := Kind,
            reason := Reason,
            exporter := ExporterModule}) ->
    {"Exporter ~tp failed to initialize with exception ~tp:~tp",
     [ExporterModule, Kind, Reason]}.
