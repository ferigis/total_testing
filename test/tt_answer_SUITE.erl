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
%% @doc Answer test suite.
%% @end
%%%-------------------------------------------------------------------
-module(tt_answer_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common test
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% Tests
-export([create_answer/1]).
-export([get_answer/1]).
-export([update_answer/1]).
-export([delete_answer/1]).

%%%===================================================================
%%% Common Test
%%%===================================================================

all() ->
  [create_answer, get_answer, update_answer, delete_answer].

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
create_answer(_Config) ->
  Test = tt_test:new(<<"Test1">>),
  {ok, TestId} = total_testing:create_test(Test),
  Question = tt_question:new(TestId, <<"This is the description of the question">>, []),
  {ok, QuestionId} = total_testing:create_question(Question),
  Question2 = tt_question:id(Question, QuestionId),
  Question2 = total_testing:get_question(QuestionId),
  Answer = tt_answer:new(QuestionId, <<"Answer Description">>, true),
  {ok, AnswerId} = total_testing:create_answer(Answer),
  Answer2 = tt_answer:id(Answer, AnswerId),
  Answer2 = total_testing:get_answer(AnswerId),
  ok = total_testing:delete_test(TestId),
  not_found = total_testing:get_answer(AnswerId).

get_answer(_Config) ->
  Test = tt_test:new(<<"Test2">>),
  {ok, TestId} = total_testing:create_test(Test),
  Question = tt_question:new(TestId, <<"This is the description of the question">>, []),
  {ok, QuestionId} = total_testing:create_question(Question),
  Answer = tt_answer:new(QuestionId, <<"Answer Description">>, true),
  {ok, AnswerId} = total_testing:create_answer(Answer),
  Answer2 = tt_answer:id(Answer, AnswerId),
  Answer2 = total_testing:get_answer(AnswerId),
  not_found = total_testing:get_answer("dfasdfsd").

update_answer(_Config) ->
  Test = tt_test:new(<<"Test3">>),
  {ok, TestId} = total_testing:create_test(Test),
  Question = tt_question:new(TestId, <<"This is the description of the question">>, []),
  {ok, QuestionId} = total_testing:create_question(Question),
  OriginalDescription = <<"Answer Description Original">>,
  Answer = tt_answer:new(QuestionId, OriginalDescription, true),
  {ok, AnswerId} = total_testing:create_answer(Answer),
  Answer2 = tt_answer:id(Answer, AnswerId),
  ModifiedDescription = <<"Question Name Modified">>,
  Answer2 = total_testing:get_answer(AnswerId),
  Answer3 = tt_answer:description(Answer2, ModifiedDescription),
  ok = total_testing:update_answer(Answer3),
  Answer3 = total_testing:get_answer(AnswerId).

delete_answer(_Config) ->
  Test = tt_test:new(<<"Test4">>),
  {ok, TestId} = total_testing:create_test(Test),
  Question = tt_question:new(TestId, <<"This is the description of the question">>, []),
  {ok, QuestionId} = total_testing:create_question(Question),
  Answer = tt_answer:new(QuestionId, <<"Answer Description">>, true),
  {ok, AnswerId} = total_testing:create_answer(Answer),
  Answer2 = tt_answer:id(Answer, AnswerId),
  Answer2 = total_testing:get_answer(AnswerId),
  ok = total_testing:delete_answer(AnswerId),
  not_found = total_testing:get_answer(AnswerId).
