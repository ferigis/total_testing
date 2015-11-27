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
%% @doc question model.
%% @end
%%%-------------------------------------------------------------------
-module(tt_question).

%% API
-export([new/3, new/4]).
-export([from_map/1]).
-export([id/1, id/2]).
-export([test_id/1, test_id/2]).
-export([description/1, description/2]).
-export([answers/1, answers/2]).

%%%===================================================================
%%% Types & Macros
%%%===================================================================

-type question()  :: #{id         => total_testing:id(),
                      test_id     => total_testing:id(),
                      description => total_testing:description(),
                      answers     => tt_answer:answer()}.

%% exported types
-export_types([question/0]).

%%====================================================================
%% API functions
%%====================================================================

-spec new(total_testing:id(), total_testing:description(),
    [tt_answer:answer()])  -> question().
new(TestId, Description, Answers) ->
  new(null, TestId, Description, Answers).

-spec new(total_testing:id(), total_testing:id(),
    total_testing:description(), [tt_answer:answer()])  -> question().
new(Id, TestId, Description, Answers) ->
  #{
    id          => Id,
    test_id     => TestId,
    description => Description,
    answers     => Answers
  }.

-spec from_map(map()) -> question().
from_map(Map) ->
  Answers = case maps:get(<<"answers">>, Map, []) of
    []      -> [];
    Result ->
      lists:map(fun tt_answer:from_map/1, Result)
  end,
  #{
    id            => maps:get(<<"id">>, Map, null),
    test_id       => maps:get(<<"test_id">>, Map, null),
    description   => maps:get(<<"description">>, Map, null),
    answers       => Answers
  }.

-spec id(question()) -> total_testing:id().
id(#{id := Val}) ->
  Val.

-spec id(question(), total_testing:id()) -> question().
id(Doc, Val) ->
  maps:put(id, Val, Doc).

-spec test_id(question()) -> total_testing:id().
test_id(#{test_id := Val}) ->
  Val.

-spec test_id(question(), total_testing:id()) -> question().
test_id(Doc, Val) ->
  maps:put(test_id, Val, Doc).

-spec description(question()) -> total_testing:description().
description(#{description := Val}) ->
  Val.

-spec description(question(), total_testing:description()) -> question().
description(Doc, Val) ->
  maps:put(description, Val, Doc).

-spec answers(question()) -> [tt_answer:answer()] | [].
answers(#{answers := Answers}) ->
  Answers.

-spec answers(question(), [tt_answer:answer()]) -> question().
answers(Doc, Val) ->
  maps:put(answers, Val, Doc).
