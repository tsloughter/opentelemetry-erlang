%%%------------------------------------------------------------------------
%% Copyright 2022, OpenTelemetry Authors
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
%% @doc
%% @end
%%%-------------------------------------------------------------------------
-module(otel_metrics_ets_sup).

-behaviour(gen_server).

-export([start_link/1,
         create_tables/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-include_lib("kernel/include/logger.hrl").

-record(state,
        {
         tables :: #{atom() => [{atom(), ets:tid()}]}
        }).

-spec start_link(otel_configuration:t()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%% @docs
-spec create_tables(atom(), [{atom(), list()}]) -> #{atom() => ets:tid()}.
create_tables(ChildId, Tables) ->
    N = gen_server:call(?MODULE, {get_tables, ChildId}),
    T = receive_or_create_tables(ChildId, maps:from_list(Tables), N),
    ct:pal("T ~p", [T]),
    T.

receive_or_create_tables(ChildId, Tables, N) ->
    receive_or_create_tables(ChildId, Tables, #{}, N).

receive_or_create_tables(ChildId, Tables, Acc, 0) ->
    create_missing_tables(ChildId, Tables, Acc);
receive_or_create_tables(ChildId, Tables, Acc, N) ->
    receive
        {'ETS-TRANSFER', Tab, _Pid, Name} ->
            ct:pal("RECEIVED ~p ~p", [Tab, Name]),
            receive_or_create_tables(ChildId, maps:remove(Name, Tables), Acc#{Name => Tab}, N-1)
    after
        0 ->
            create_missing_tables(ChildId, Tables, Acc)
    end.

create_missing_tables(ChildId, Tables, TableAcc) ->
    Pid = erlang:whereis(?MODULE),
    maps:fold(fun(Name, Options, Acc) ->
                      try
                          Acc#{Name => ets:new(Name, [{heir, Pid, ChildId} | Options])}
                      catch
                          _C:_T:_S ->
                              Acc
                      end
              end, TableAcc, Tables).

init(_) ->
    {ok, #state{tables=#{}}}.

handle_call({get_tables, ChildId}, {Pid, _}, State=#state{tables=Tables}) ->
    ChildTables = maps:get(ChildId, Tables, #{}),
    Num = maps:fold(fun(Name, Tab, Acc) ->
                            ets:give_away(Tab, Pid, Name),
                            Acc+1
                    end, 0, ChildTables),
    {reply, Num, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'ETS-TRANSFER', Tab, _FromPid, ChildId}, State=#state{tables=Tables}) ->
    ChildTables = maps:get(ChildId, Tables, #{}),
    {noreply, State#state{tables=Tables#{ChildId => ChildTables#{ets:info(Tab, name) => Tab}}}}.
