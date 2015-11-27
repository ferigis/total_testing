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
%% @doc questions test suite.
%% @end
%%%-------------------------------------------------------------------
-module(tt_questions_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common test
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% Tests
-export([create_question/1]).
-export([get_question/1]).
-export([update_question/1]).
-export([delete_question/1]).

%%%===================================================================
%%% Common Test
%%%===================================================================

all() ->
  [create_question, get_question, update_question, delete_question].

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
create_question(_Config) ->
  Test = tt_test:new(<<"Test1">>),
  {ok, TestId} = total_testing:create_test(Test),
  Question = tt_question:new(TestId, <<"This is the description of the question">>, []),
  Answer1 = tt_answer:new(null, <<"Answer 1 Description">>, false),
  Answer2 = tt_answer:new(null, <<"Answer 2 Description">>, true),
  Question2 = tt_question:answers(Question, [Answer1, Answer2]),
  {ok, QuestionId} = total_testing:create_question(Question2),
  Question3 = total_testing:get_question(QuestionId),
  [Answer3, _] = tt_question:answers(Question3),
  Answer3Id = tt_answer:id(Answer3),
  Answer3 = total_testing:get_answer(Answer3Id),
  ok = total_testing:delete_test(TestId),
  not_found = total_testing:get_question(QuestionId),
  not_found = total_testing:get_answer(Answer3Id).

get_question(_Config) ->
  Test = tt_test:new(<<"Test2">>),
  {ok, TestId} = total_testing:create_test(Test),
  Question = tt_question:new(TestId, <<"This is the description of the question">>, []),
  {ok, QuestionId} = total_testing:create_question(Question),
  Question2 = tt_question:id(Question, QuestionId),
  Question2 = total_testing:get_question(QuestionId),
  not_found = total_testing:get_question("dfasdfa").

update_question(_Config) ->
  Test = tt_test:new(<<"Test3">>),
  {ok, TestId} = total_testing:create_test(Test),
  OriginalDescription = <<"Question Description Original">>,
  Question = tt_question:new(TestId, OriginalDescription, []),
  Answer1 = tt_answer:new(null, <<"Answer 1 Description">>, false),
  Answer2 = tt_answer:new(null, <<"Answer 2 Description">>, true),
  Question1 = tt_question:answers(Question, [Answer1, Answer2]),
  {ok, QuestionId} = total_testing:create_question(Question1),
  ModifiedDescription = <<"Question Name Modified">>,
  Question2 = total_testing:get_question(QuestionId),
  2 = length(tt_question:answers(Question2)),
  Question3 = tt_question:description(Question2, ModifiedDescription),
  Question4 = tt_question:answers(Question3, [Answer1]),
  ok = total_testing:update_question(Question4),
  Question5 = total_testing:get_question(QuestionId),
  1 = length(tt_question:answers(Question5)).

delete_question(_Config) ->
  Test = tt_test:new(<<"Test4">>),
  {ok, TestId} = total_testing:create_test(Test),
  Question = tt_question:new(TestId, <<"This is the description of the question">>, []),
  {ok, QuestionId} = total_testing:create_question(Question),
  Answer1 = tt_answer:new(QuestionId, <<"Answer 1 Description">>, false),
  {ok, AnswerId} = total_testing:create_answer(Answer1),
  Answer2 = total_testing:get_answer(AnswerId),
  Question2 = total_testing:get_question(QuestionId),
  [Answer2] = tt_question:answers(Question2),
  ok = total_testing:delete_question(QuestionId),
  not_found = total_testing:get_question(QuestionId),
  not_found = total_testing:get_answer(AnswerId).
