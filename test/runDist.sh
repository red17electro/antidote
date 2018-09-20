#!/usr/bin/env bash

cd ..
make cleantests
make compile-utils
cd test

antidote_test_utils=$(pwd)/utils \
    antidote_ebin=$(pwd)/../_build/default/lib/*/ebin \
    riak_schema_dirs=$(pwd)/../_build/default/rel/antidote/lib/ \
    distributed=true \
    append_SUITE_dev1=21111 \
    append_SUITE_dev2=21211 \
    append_SUITE_dev3=21311 \
    append_SUITE_dev4=21411 \
    antidote_SUITE_dev1=22111 \
    antidote_SUITE_dev2=22211 \
    antidote_SUITE_dev3=22311 \
    antidote_SUITE_dev4=22411 \
    bcountermgr_SUITE_dev1=23111 \
    bcountermgr_SUITE_dev2=23211 \
    bcountermgr_SUITE_dev3=23311 \
    bcountermgr_SUITE_dev4=23411 \
    clocksi_SUITE_dev1=24111 \
    clocksi_SUITE_dev2=24211 \
    clocksi_SUITE_dev3=24311 \
    clocksi_SUITE_dev4=24411 \
    gr_SUITE_dev1=26111 \
    gr_SUITE_dev2=26211 \
    gr_SUITE_dev3=26311 \
    gr_SUITE_dev4=26411 \
    inter_dc_repl_SUITE_dev1=27111 \
    inter_dc_repl_SUITE_dev2=27211 \
    inter_dc_repl_SUITE_dev3=27311 \
    inter_dc_repl_SUITE_dev4=27411 \
    multiple_dcs_node_failure_SUITE_dev1=29111 \
    multiple_dcs_node_failure_SUITE_dev2=29211 \
    multiple_dcs_node_failure_SUITE_dev3=29311 \
    multiple_dcs_node_failure_SUITE_dev4=29411 \
    multiple_dcs_SUITE_dev1=30111 \
    multiple_dcs_SUITE_dev2=30211 \
    multiple_dcs_SUITE_dev3=30311 \
    multiple_dcs_SUITE_dev4=30411 \
    erl -name ct_master \
    -pa ../_build/default/lib/*/ebin \
    -pa utils
