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
%% @doc total testing API.
%% @end
%%%-------------------------------------------------------------------
-module(total_testing).

%% API
-export([start/0]).

%% Test API
-export([create_test/1]).
-export([get_test/0, get_test/1]).
-export([update_test/1]).
-export([delete_test/1]).

%% Question API
-export([create_question/1]).
-export([get_question/1]).
-export([get_questions_by_test/1]).
-export([get_random_questions/1]).
-export([update_question/1]).
-export([delete_question/1]).

%% Answer API
-export([create_answer/1]).
-export([get_answer/1]).
-export([update_answer/1]).
-export([delete_answer/1]).

%%%===================================================================
%%% Types & Macros
%%%===================================================================


%% Total testing types
-type id()          :: iolist().
-type name()        :: iolist().
-type description() :: iolist().

%% exported types
-export_types([id/0]).
-export_types([name/0]).
-export_types([description/0]).

%%====================================================================
%% API functions
%%====================================================================

start() ->
  tt_app:start().

%% TEST API

%% @doc Create a new test
-spec create_test(tt_test:test()) -> {ok, id()}.
create_test(Test) when is_map(Test) ->
  Test2 = tt_test:id(Test, tt_util:generate_id()),
  tt_test_repo:set(Test2).

%% @doc Get all
-spec get_test() -> [tt_test:test()].
get_test() ->
  tt_test_repo:get().

%% @doc Get a test by Id
-spec get_test(id()) -> tt_test:test() | not_found.
get_test(TestId) ->
  tt_test_repo:get(TestId).

%% @doc update a test
-spec update_test(tt_test:test()) -> ok.
update_test(Test) when is_map(Test) ->
  tt_test_repo:update(Test).

%% @doc delete a test by id
-spec delete_test(id()) -> ok.
delete_test(TestId) ->
  ok = tt_test_repo:delete(TestId),
  case tt_question_repo:get_by_test(TestId) of
    [] -> ok;
    Result ->
      F = fun(Question) -> delete_question(tt_question:id(Question)) end,
      lists:map(F, Result),
      ok
  end.

%% QUESTION API

%% @doc Create a new question
-spec create_question(tt_question:question()) -> {ok, id()}.
create_question(Question) when is_map(Question) ->
  Question2 = tt_question:id(Question, tt_util:generate_id()),
  {ok, QuestionId} = tt_question_repo:set(Question2),
  ok = save_answers_by_question(Question2),
  {ok, QuestionId}.

%% @doc Get a question by Id
-spec get_question(id()) -> tt_question:question() | not_found.
get_question(QuestionId) ->
  case tt_question_repo:get(QuestionId) of
    not_found ->
      not_found;
    Question ->
      tt_question:answers(Question, tt_answer_repo:get_by_question(QuestionId))
  end.

-spec get_questions_by_test(total_testing:id()) -> [tt_question:question()] | [].
get_questions_by_test(TestId) ->
  tt_question_repo:get_by_test(TestId).

-spec get_random_questions(integer()) -> [tt_question:question()] | [].
get_random_questions(Number) when is_integer(Number) and (Number > 0) ->
  tt_question_repo:get_random(Number).

%% @doc update a question
-spec update_question(tt_question:question()) -> ok.
update_question(Question) when is_map(Question) ->
  tt_question_repo:update(Question),
  delete_answer_by_question(tt_question:id(Question)),
  save_answers_by_question(Question).

%% @doc delete a question by id
-spec delete_question(id()) -> ok.
delete_question(QuestionId) ->
  tt_question_repo:delete(QuestionId),
  delete_answer_by_question(QuestionId).

%% ANSWER API

%% @doc Create a new answer
-spec create_answer(tt_answer:answer()) -> {ok, id()}.
create_answer(Answer) when is_map(Answer) ->
  Answer2 = tt_answer:id(Answer, tt_util:generate_id()),
  tt_answer_repo:set(Answer2).

%% @doc Get a answer by Id
-spec get_answer(id()) -> tt_answer:answer().
get_answer(AnswerId) ->
  tt_answer_repo:get(AnswerId).

%% @doc update a answer
-spec update_answer(tt_answer:answer()) -> ok.
update_answer(Answer) when is_map(Answer) ->
  tt_answer_repo:update(Answer).

%% @doc delete a answer by id
-spec delete_answer(id()) -> ok.
delete_answer(AnswerId) ->
  tt_answer_repo:delete(AnswerId).

%%====================================================================
%% Internal functions
%%====================================================================

-spec delete_answer_by_question(id()) -> ok.
delete_answer_by_question(QuestionId) ->
  case tt_answer_repo:get_by_question(QuestionId) of
    []      -> ok;
    Result  ->
      F = fun(Answer) -> delete_answer(tt_answer:id(Answer)) end,
      lists:map(F, Result),
      ok
  end.

-spec save_answers_by_question(tt_question:question()) -> ok.
save_answers_by_question(Question) ->
  case tt_question:answers(Question) of
    []      -> ok;
    Answers ->
      F = fun(Answer) ->
        Answer2 = tt_answer:question_id(Answer, tt_question:id(Question)),
        create_answer(Answer2)
      end,
      lists:map(F, Answers),
      ok
  end.
