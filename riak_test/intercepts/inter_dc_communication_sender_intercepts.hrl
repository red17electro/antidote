-module(inter_dc_communication_sender_intercepts).
-compile(export_all).
-include("intercept.hrl").
-define(M, inter_dc_communication_sender_orig).


%% TestInt1:
%% For this test, want to have an update to key5 at DC1
%% which doesn't replicate key5
%% This intercept will block the sending of the key
%% When the client tries to read his own writes it will
%% fail because no-one has a copy of this key that is enough
%% up to date
%% Could make this succeed by caching, but that is a future optimization
%% Ports 90021 - 90023 are used, only intercept them
%%
%% TestInt2:
%% For this test, update to a key5 at DC1
%% which doesn't replicate key5, other DCs should replicate it
%% Only let this propagate to 1 DC (the DC with port 90033)
%% When client tries to read his write he should still succeed
%% because at least one DC will have a replica
%% Note: updates are only sent to the DC with the largest Port number
%% which will be the last attempt to read this value given the test setup
%% Ports 90031 - 90033
fail_send_propagate_sync(DictTransactionsDcs, StableTime, Partition) ->
    NewDict = dict:foldl(fun(DcList, ListTrans, NewDict) ->
				 %% Check for the right ports
				 %% Again change the ports so the propagation
				 %% just times out
				 NewDcList = list:foldl(fun({Address,Port}, AccList) ->
								NewPort = case Port of
									      APort when Aport > 90020, Aport < 90024 ->
										  ?I_INFO("Changed port in Propagate_sync from ~p to ~p for TestInt1", [Port, 90000]),
										  90000;
									      BPort when Bport > 90030, Bport < 90033 ->
										  ?I_INFO("Changed port in Propagate_sync from ~p to ~p for TestInt2", [Port, 90000]),
										  90000;
									      _ ->
										  Port
									  end,
								[{Address,NewPort}, AccList]
							end,
							[], DcList),
				 dict:store(NewDcList, ListTrans, NewDict)
			 end,
			 dict:new(), DictTransactionsDcs),
    ?M:fail_send_propagate_sync_orig(NewDict, StableTime, Partition).
    

%% TestInt3:
%% Partition the DCs so that they dont recieve information to propagate
%% their clocks
%% But the local DC should still be able to conintue operations
partition_dc_propagate_sync_safe_time({DcAddress, Port}, Transaction) ->
    %% Ports 90011 - 90013 are used for this test, so only intercept those ports
    %% Set them to a new port that should just timeout
    NewPort = case Port of
		  APort when Aport > 90010, Aport < 90014 ->
		      ?I_INFO("Changed port in Propagate_sync_safe_time from ~p to ~p", [Port, 90000]),
		      90000;
		  _ ->
		      Port;
		  end,
    ?M:propagate_sync_safe_time_org({DcAddress, NewPort, Transaction).
