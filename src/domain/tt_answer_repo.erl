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
%% @doc Answers repository
%% @end
%%%-------------------------------------------------------------------
-module(tt_answer_repo).

%% API
-export([init/0]).
-export([get/1]).
-export([get_by_question/1]).
-export([set/1]).
-export([update/1]).
-export([delete/1]).

%%%===================================================================
%%% Types & Macros
%%%===================================================================

%% mnesia record
-record(answer, {id           :: total_testing:id(),
                  question_id :: total_testing:id(),
							    description :: total_testing:description(),
                  is_correct  :: boolean()}).

%% types
-type answer() :: #answer{}.


%%====================================================================
%% API functions
%%====================================================================

%% @doc initialize the answer table in mnesia
-spec init() -> ok.
init() ->
  mnesia:create_table(answer,
                        [{attributes, record_info(fields, answer)},
                        {disc_copies, [node()]}
                        ]).

%% @doc get answer by id
-spec get(total_testing:id()) -> total_testing:answer() | not_found.
get(AnswerId) ->
	case mnesia:dirty_read(answer, AnswerId) of
    [] -> not_found;
    [Answer] ->
      record_to_answer(Answer)
  end.

-spec get_by_question(total_testing:id() | tt_question:question()) -> [tt_question:question()] | [].
get_by_question(Question) when is_map(Question) ->
  get_by_question(tt_question:id(Question));
get_by_question(QuestionId) ->
  Answer = #answer{_ = '_', question_id = QuestionId},
  F = fun() ->
    mnesia:match_object(Answer)
  end,
  case mnesia:activity(transaction, F) of
    []      -> [];
    Result  ->
      lists:map(fun record_to_answer/1, Result)
  end.

%% @doc create a new answer
-spec set(tt_answer:answer()) -> {ok, total_testing:id()}.
set(Answer) when is_map(Answer) ->
	save(Answer).

%% @doc update a answer
-spec update(tt_answer:answer()) -> ok.
update(Answer) when is_map(Answer) ->
	{ok, _} = save(Answer),
	ok.

%% @doc delete a answer by id
-spec delete(total_testing:id()) -> ok.
delete(Answer) ->
	F = fun() ->
    mnesia:delete({answer, Answer})
  end,
  mnesia:activity(transaction, F),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
-spec answer_to_record(tt_answer:answer()) -> answer().
answer_to_record(#{id := Id, question_id := QuestionId,
                    description := Description, is_correct := IsCorrect}) ->
	#answer{id = Id, question_id = QuestionId, description = Description, is_correct = IsCorrect}.

%% @private
-spec record_to_answer(answer()) -> tt_answer:answer().
record_to_answer(Answer) when is_record(Answer, answer) ->
	tt_answer:new(Answer#answer.id, Answer#answer.question_id,
    Answer#answer.description,Answer#answer.is_correct).

%% @private
-spec save(tt_answer:answer()) -> {ok, total_testing:id()}.
save(Answer) when is_map(Answer) ->
  AnswerRecord = answer_to_record(Answer),
	F = fun() ->
    mnesia:write(answer, AnswerRecord, write)
  end,
  mnesia:activity(transaction, F),
  {ok, AnswerRecord#answer.id}.
