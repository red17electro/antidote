%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 SyncFree Consortium.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(bigset).

-compile({parse_transform, lager_transform}).

%% common_test callbacks
-export([%% suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0]).

-export([multiple_writes/3,
	 replicated_set_test/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/inet.hrl").

-define(BUCKET, "multiple_dcs").

init_per_suite(Config) ->
    test_utils:at_init_testsuite(),
    Clusters = test_utils:set_up_clusters_common(Config),
    Nodes = lists:flatten(Clusters),

    %Ensure that the clocksi protocol is used
    test_utils:pmap(fun(Node) ->
        rpc:call(Node, application, set_env,
        [antidote, txn_prot, clocksi]) end, Nodes),

    %Check that indeed clocksi is running
    {ok, clocksi} = rpc:call(hd(hd(Clusters)), application, get_env, [antidote, txn_prot]),

    [{clusters, Clusters}|Config].

end_per_suite(Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_, _) ->
    ok.

all() ->
    [
     replicated_set_test].

multiple_writes(Node, Key, ReplyTo) ->
    update_counters(Node, [Key], [1], ignore, static),
    update_counters(Node, [Key], [1], ignore, static),
    update_counters(Node, [Key], [1], ignore, static),
    update_counters(Node, [Key], [1], ignore, static),
    {ok, CommitTime} = update_counters(Node, [Key], [1], ignore, static),
    ReplyTo ! {ok, CommitTime}.


replicated_set_test(Config) ->
    Clusters = proplists:get_value(clusters, Config),
    [Node1, Node2 | _Nodes] =  [ hd(Cluster)|| Cluster <- Clusters ],

    Key1 = replicated_set_test,
    Type = antidote_crdt_bigset,

    lager:info("Writing 100 elements to set!!!"),

    %% add 100 elements to the set on Node 1 while simultaneously reading on Node2
    CommitTimes = lists:map(fun(N) ->
				    lager:info("Writing ~p to set", [N]),
				    {ok, CommitTime} = update_sets(Node1, [Key1], [{add, N}], ignore),
				    timer:sleep(200),
				    CommitTime
			    end, lists:seq(1, 5)),

    LastCommitTime = lists:last(CommitTimes),
    lager:info("last commit time was ~p.", [LastCommitTime]),

    %% now read on Node2
    %%check_read_key(Node2, Key1, Type, lists:seq(1,5), LastCommitTime, static),
    pass.

%% internal
check_read_key(Node, Key, Type, Expected, Clock, TxId) ->
    check_read(Node, [{Key, Type, ?BUCKET}], [Expected], Clock, TxId).

check_read(Node, Objects, Expected, Clock, TxId) ->
    case TxId of
        static ->
            {ok, Res, CT} = rpc:call(Node, cure, read_objects, [Clock, [], Objects]),
            ?assertEqual(Expected, Res),
            {ok, Res, CT};
        _ ->
            {ok, Res} = rpc:call(Node, cure, read_objects, [Objects, TxId]),
            ?assertEqual(Expected, Res),
            {ok, Res}
    end.

update_counters(Node, Keys, IncValues, Clock, TxId) ->
    Updates = lists:map(fun({Key, Inc}) ->
                                {{Key, antidote_crdt_counter, ?BUCKET}, increment, Inc}
                        end,
                        lists:zip(Keys, IncValues)
                       ),

    case TxId of
        static ->
            {ok, CT} = rpc:call(Node, cure, update_objects, [Clock, [], Updates]),
            {ok, CT};
        _->
            ok = rpc:call(Node, cure, update_objects, [Updates, TxId]),
            ok
    end.

update_sets(Node, Keys, Ops, Clock) ->
    Updates = lists:map(fun({Key, {Op, Param}}) ->
                                {{Key, antidote_crdt_bigset, ?BUCKET}, Op, Param}
                        end,
                        lists:zip(Keys, Ops)
                       ),
    {ok, CT} = rpc:call(Node, antidote, update_objects, [Clock, [], Updates]),
    {ok, CT}.
