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
-module(clocksi_readitem_fsm).

-behavior(gen_fsm).

-include("antidote.hrl").

%% API
-export([start_link/7]).

%% Callbacks
-export([init/1,
         code_change/4,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4,
         terminate/3]).

%% States
-export([check_clock/2,
         waiting1/2,
         return/2]).

%% Spawn

-record(state, {type,
                key,
                transaction,
                tx_coordinator,
                vnode,
                updates,
                pending_txs,
		isLocal}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Vnode, Coordinator, Tx, Key, Type, Updates, IsLocal) ->
    gen_fsm:start_link(?MODULE, [Vnode, Coordinator,
                                 Tx, Key, Type, Updates, IsLocal], []).

now_milisec({MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs.

%%%===================================================================
%%% States
%%%===================================================================

init([Vnode, Coordinator, Transaction, Key, Type, Updates, local]) ->
    SD = #state{vnode=Vnode,
                type=Type,
                key=Key,
                tx_coordinator=Coordinator,
                transaction=Transaction,
                updates=Updates,
                pending_txs=[],
		isLocal=true},
    {ok, check_clock, SD, 0};

init([Vnode, Coordinator, Transaction, Key, Type, Updates, external]) ->
    SD = #state{vnode=Vnode,
                type=Type,
                key=Key,
                tx_coordinator=Coordinator,
                transaction=Transaction,
                updates=Updates,
                pending_txs=[],
		isLocal=false},
    {ok, check_clock, SD, 0}.


%% @doc check_clock: Compares its local clock with the tx timestamp.
%%      if local clock is behinf, it sleeps the fms until the clock
%%      catches up. CLOCK-SI: clock skew.
%%
%% For partial-rep, when a read is coming from another DC it also
%% waits until all the dependencies of that read are satisfied
check_clock(timeout, SD0=#state{transaction=Transaction,isLocal=IsLocal}) ->
    TxId = Transaction#transaction.txn_id,
    T_TS = TxId#tx_id.snapshot_time,
    Time = now_milisec(erlang:now()),
    case IsLocal of
	true -> 
	    case (T_TS) > Time of
		true ->
		    timer:sleep(T_TS - Time),
		    {next_state, waiting1, SD0, 0};
		false ->
		    {next_state, waiting1, SD0, 0}
	    end;
	false ->
	    %% This could cause really long waiting, because the time of the
	    %% origin DC is using time:now() for its time, instead of using
	    %% time based on one of its dependencies
	    %% Should fix this
	    vectorclock:wait_for_clock(Transaction#transaction.vec_snapshot_time),
	    {next_state, return, SD0, 0}
    end.

waiting1(timeout, SDO=#state{key=Key, transaction=Transaction}) ->
    LocalClock = get_stable_time(Key),
    TxId = Transaction#transaction.txn_id,
    SnapshotTime = TxId#tx_id.snapshot_time,
    case LocalClock > SnapshotTime of
        false ->
            {next_state, waiting1, SDO, 1};
        true ->
            {next_state, return, SDO, 0}
    end;

waiting1(_SomeMessage, SDO) ->
    {next_state, waiting1, SDO,0}.

%% @doc return:
%%  - Reads and returns the log of specified Key using replication layer.
return(timeout, SD0=#state{key=Key,
                           tx_coordinator=Coordinator,
                           transaction=Transaction,
                           type=Type,
                           updates=Updates}) ->
    VecSnapshotTime = Transaction#transaction.vec_snapshot_time,
    case materializer_vnode:read(Key, Type, VecSnapshotTime) of
        {ok, Snapshot} ->
            Updates2=filter_updates_per_key(Updates, Key),
            Snapshot2=clocksi_materializer:update_snapshot_eager
                        (Type, Snapshot, Updates2),
            Reply = {ok, Snapshot2};
        {error, Reason} ->
            Reply={error, Reason}
    end,
    riak_core_vnode:reply(Coordinator, Reply),
    {stop, normal, SD0};
return(_SomeMessage, SDO) ->
    {next_state, return, SDO,0}.

handle_info(_Info, StateName, StateData) ->
    {next_state,StateName,StateData,1}.

handle_event(_Event, _StateName, StateData) ->
    {stop,badmsg,StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop,badmsg,StateData}.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

terminate(_Reason, _SN, _SD) ->
    ok.

%% Internal functions

filter_updates_per_key(Updates, Key) ->
    int_filter_updates_key(Updates, Key, []).

int_filter_updates_key([], _Key, Updates2) ->
    Updates2;

int_filter_updates_key([Next|Rest], Key, Updates2) ->
    {_, {KeyPrime, _Type, Op}} = Next,
    case KeyPrime==Key of
        true ->
            int_filter_updates_key(Rest, Key, lists:append(Updates2, [Op]));
        false ->
            int_filter_updates_key(Rest, Key, Updates2)
    end.

get_stable_time(Key) ->
    Preflist = log_utilities:get_preflist_from_key(Key),
    Node = hd(Preflist),
    case riak_core_vnode_master:sync_command(
           Node, {get_active_txns}, ?CLOCKSI_MASTER) of
        {ok, Active_txns} ->
            lists:foldl(fun({_,{_TxId, Snapshot_time}}, Min_time) ->
                                case Min_time > Snapshot_time of
                                    true ->
                                        Snapshot_time;
                                    false ->
                                        Min_time
                                end
                        end,
                        now_milisec(erlang:now()),
                        Active_txns);
        _ -> 0
    end.
