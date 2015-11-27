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
%% @doc Questions repository
%% @end
%%%-------------------------------------------------------------------
-module(tt_question_repo).

%% API
-export([init/0]).
-export([get/1]).
-export([get_by_test/1]).
-export([get_random/1]).
-export([set/1]).
-export([set_answers/1]).
-export([update/1]).
-export([delete/1]).

%%%===================================================================
%%% Types & Macros
%%%===================================================================

%% mnesia record
-record(question, {id         :: total_testing:id(),
                  test_id     :: total_testing:id(),
							    description :: total_testing:description()}).

%% types
-type question() :: #question{}.


%%====================================================================
%% API functions
%%====================================================================

%% @doc initialize the question table in mnesia
-spec init() -> ok.
init() ->
  mnesia:create_table(question,
                        [{attributes, record_info(fields, question)},
                        {index, [#question.test_id]},
                        {disc_copies, [node()]}
                        ]).

%% @doc get question by id
-spec get(total_testing:id()) -> tt_question:question() | not_found.
get(QuestionId) ->
	case mnesia:dirty_read(question, QuestionId) of
    [] -> not_found;
    [Question] ->
      record_to_question(Question)
  end.

-spec get_by_test(total_testing:id()) -> [tt_question:question()] | [].
get_by_test(TestId) ->
  Question = #question{_ = '_', test_id = TestId},
  F = fun() ->
    mnesia:match_object(Question)
  end,
  case mnesia:activity(transaction, F) of
    []      -> [];
    Result  ->
      Result2 = lists:map(fun record_to_question/1, Result),
      lists:map(fun set_answers/1, Result2)

  end.

-spec get_random(integer()) -> [tt_question:question()] | [].
get_random(Number) when is_integer(Number) and (Number > 0) ->
  Keys = mnesia:dirty_all_keys(question),
  case length(Keys) =< Number of
    true  -> get_all();
    false ->
      Questions = get_random(Keys, [], Number),
      lists:map(fun set_answers/1, Questions)
  end.

%% @doc create a new question
-spec set(tt_question:question()) -> {ok, total_testing:id()}.
set(Question) when is_map(Question) ->
	save(Question).

-spec set_answers(tt_question:question()) -> tt_question:question().
set_answers(Question) ->
  tt_question:answers(Question, tt_answer_repo:get_by_question(Question)).

%% @doc update a question
-spec update(tt_question:question()) -> ok.
update(Question) when is_map(Question) ->
	{ok, _} = save(Question),
	ok.

%% @doc delete a question by id
-spec delete(total_testing:id()) -> ok.
delete(QuestionId) ->
	F = fun() ->
    mnesia:delete({question, QuestionId})
  end,
  mnesia:activity(transaction, F),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
-spec question_to_record(tt_question:question()) -> question().
question_to_record(#{id := Id, test_id := TestId, description := Description}) ->
	#question{id = Id, test_id = TestId, description = Description}.

%% @private
-spec record_to_question(question()) -> tt_question:question().
record_to_question(Question) when is_record(Question, question) ->
	tt_question:new(Question#question.id,
    Question#question.test_id, Question#question.description, []).

%% @private
-spec save(tt_question:question()) -> {ok, total_testing:id()}.
save(Question) when is_map(Question) ->
	QuestionRecord = question_to_record(Question),
	F = fun() ->
    mnesia:write(question, QuestionRecord, write)
  end,
  mnesia:activity(transaction, F),
  {ok, QuestionRecord#question.id}.

%% @private
-spec get_all() -> [tt_question:question()] | [].
get_all() ->
  F = fun() -> mnesia:select(question,[{'_',[],['$_']}]) end,
  case mnesia:activity(transaction, F) of
    []      -> [];
    Result  ->
      Result2 = lists:map(fun record_to_question/1, Result),
      lists:map(fun set_answers/1, Result2)
  end.

%% @private
-spec get_random(list(), [tt_question:question()], integer()) -> [tt_question:question()] | [].
get_random(_, Result, 0) ->
  Result;
get_random(Keys, Result, Number) ->
  Key = lists:nth(random:uniform(length(Keys)), Keys),
  Question = ?MODULE:get(Key),
  get_random(lists:delete(Question, Keys), [Question | Result], Number - 1).
