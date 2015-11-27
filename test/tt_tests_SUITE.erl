%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Felipe Ripoll, Inc. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% @author Felipe Ripoll <ferigis@gmail.com>
%%% @copyright (C) 2015, <Felipe Ripoll>, All Rights Reserved.
%% @doc tests test suite.
%% @end
%%%-------------------------------------------------------------------
-module(tt_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common test
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% Tests
-export([create_test/1]).
-export([get_test/1]).
-export([update_test/1]).
-export([delete_test/1]).

%%%===================================================================
%%% Common Test
%%%===================================================================

all() ->
  [create_test, get_test, update_test, delete_test].

init_per_suite(Config) ->
  total_testing:start(),
  Config.

end_per_suite(Config) ->
  tt_test_util:clear_backend(),
  ok = application:stop(total_testing),
  Config.

%%%===================================================================
%%% Exported Tests Functions
%%%===================================================================

create_test(_Config) ->
  Test = tt_test:new(<<"Test1">>),
  {ok, TestId} = total_testing:create_test(Test),
  Test2 = tt_test:id(Test, TestId),
  Test3 = tt_test:questions_number(Test2, 0),
  Test3 = total_testing:get_test(TestId).

get_test(_Config) ->
  Test = tt_test:new(<<"Test2">>),
  {ok, TestId} = total_testing:create_test(Test),
  Test2 = tt_test:id(Test, TestId),
  Test3 = tt_test:questions_number(Test2, 0),
  Test3 = total_testing:get_test(TestId),
  not_found = total_testing:get_test("fsadfsadfasdf").

update_test(_Config) ->
  OriginalName = <<"Test Name Original">>,
  Test = tt_test:new(OriginalName),
  {ok, TestId} = total_testing:create_test(Test),
  ModifiedName = <<"Test Name Modified">>,
  Test2 = total_testing:get_test(TestId),
  Test3 = tt_test:name(Test2, ModifiedName),
  ok = total_testing:update_test(Test3),
  Test3 = total_testing:get_test(TestId).

delete_test(_Config) ->
  Test = tt_test:new(<<"Test3">>),
  {ok, TestId} = total_testing:create_test(Test),
  Test2 = tt_test:id(Test, TestId),
  Test3 = tt_test:questions_number(Test2, 0),
  Test3 = total_testing:get_test(TestId),
  ok = total_testing:delete_test(TestId),
  not_found = total_testing:get_test(TestId).
