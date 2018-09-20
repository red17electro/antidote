%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Helium Systems, Inc.  All Rights Reserved.
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

-module(network_utils).

-export([
    web_port/1
]).


-spec web_port(node()) -> integer().
%% @doc Backwards-compatible web port implementation for non-distributed testing
web_port(dev1) -> 10015;
web_port(dev2) -> 10025;
web_port(dev3) -> 10035;
web_port(dev4) -> 10045;

%% @doc Fetches free ports for given test suite and node
%%      Read environment variables of format SUITE_NODE=PORT if 'distributed' testing is enabled
web_port(Node) ->
    %% only enable environment variables for distributed testing
    IsDistributed = os:getenv("distributed", undefined),
    case IsDistributed of
        undefined -> web_port(Node);
        _ -> distributed_web_port(Node)
    end.


-spec distributed_web_port(node()) -> integer().
distributed_web_port(Node) ->
    EnvPort = os:getenv(atom_to_list(Node), undefined),

    case EnvPort of
        undefined ->
            ct:pal("No port defined for node '~p'. Provide environment variable '~p=PORT'.", [Node, Node]),
            ct:fail("No port defined");
        _ ->
            Port = list_to_integer(EnvPort),
            ct:pal("Using port ~p for ~p", [Port, Node]),
            Port
    end.
