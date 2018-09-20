{node, slave2, 'ct_slave2@think1.local'}.
{node, slave3, 'ct_slave3@think1.local'}.
{node, slave4, 'ct_slave4@think1.local'}.
{node, slave5, 'ct_slave5@think1.local'}.
{node, slave6, 'ct_slave6@think1.local'}.
{node, slave8, 'ct_slave8@think1.local'}.

%{node, slave1, 'ct_slave1@think1.local'}.
%{node, slave7, 'ct_slave7@think1.local'}.

{alias, antidote_systests, "multidc"}.

{logdir, all_nodes, "/home/schimpf/logs/"}.
{logdir, master, "/home/schimpf/logs/"}.


{init, slave2, [{node_start, [{monitor_master, true}, {erl_flags, "-pa $antidote_test_utils -pa $antidote_ebin"}]}]}.
{init, slave3, [{node_start, [{monitor_master, true}, {erl_flags, "-pa $antidote_test_utils -pa $antidote_ebin"}]}]}.
{init, slave4, [{node_start, [{monitor_master, true}, {erl_flags, "-pa $antidote_test_utils -pa $antidote_ebin"}]}]}.
{init, slave5, [{node_start, [{monitor_master, true}, {erl_flags, "-pa $antidote_test_utils -pa $antidote_ebin"}]}]}.
{init, slave6, [{node_start, [{monitor_master, true}, {erl_flags, "-pa $antidote_test_utils -pa $antidote_ebin"}]}]}.
{init, slave8, [{node_start, [{monitor_master, true}, {erl_flags, "-pa $antidote_test_utils -pa $antidote_ebin"}]}]}.

%{init, slave1, [{node_start, [{monitor_master, true}, {erl_flags, "-pa $antidote_test_utils -pa $antidote_ebin"}]}]}.
%{init, slave7, [{node_start, [{monitor_master, true}, {erl_flags, "-pa $antidote_test_utils -pa $antidote_ebin"}]}]}.

% join clusters does not work?
% {suites, [slave1], antidote_systests, [append_SUITE]}.
{suites, [slave3], antidote_systests, [antidote_SUITE]}.
{suites, [slave4], antidote_systests, [bcountermgr_SUITE]}.
{suites, [slave5], antidote_systests, [clocksi_SUITE]}.
{suites, [slave2], antidote_systests, [gr_SUITE]}.
{suites, [slave6], antidote_systests, [inter_dc_repl_SUITE]}.
{suites, [slave8], antidote_systests, [multiple_dcs_SUITE]}.
% some rpc timeout
%{suites, [slave7], antidote_systests, [multiple_dcs_node_failure_SUITE]}.
