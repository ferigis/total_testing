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
%% @doc answer model.
%% @end
%%%-------------------------------------------------------------------
-module(tt_answer).

%% API
-export([new/3, new/4]).
-export([from_map/1]).
-export([id/1, id/2]).
-export([question_id/1, question_id/2]).
-export([description/1, description/2]).
-export([is_correct/1, is_correct/2]).

%%%===================================================================
%%% Types & Macros
%%%===================================================================

-type answer()  :: #{id         => total_testing:id(),
                    question_id => total_testing:id(),
                    description => total_testing:description(),
                    is_correct  => boolean()}.

%% exported types
-export_types([answer/0]).

%%====================================================================
%% API functions
%%====================================================================

-spec new(total_testing:id(),
    total_testing:description(),
    boolean())  -> answer().
new(QuestionId, Description, IsCorrect) ->
  new(null, QuestionId, Description, IsCorrect).

-spec new(total_testing:id(),
          total_testing:id(),
          total_testing:description(),
          boolean())  -> answer().
new(Id, QuestionId, Description, IsCorrect) ->
  #{
    id          => Id,
    question_id => QuestionId,
    description => Description,
    is_correct  => IsCorrect
  }.

-spec from_map(map()) -> answer().
from_map(Map) ->
  #{
    id            => maps:get(<<"id">>, Map, null),
    question_id   => maps:get(<<"question_id">>, Map, null),
    description   => maps:get(<<"description">>, Map, null),
    is_correct    => maps:get(<<"is_correct">>, Map, false)
  }.

-spec id(answer()) -> total_testing:id().
id(#{id := Val}) ->
  Val.

-spec id(answer(), total_testing:id()) -> answer().
id(Doc, Val) ->
  maps:put(id, Val, Doc).

-spec question_id(answer()) -> total_testing:id().
question_id(#{question_id := Val}) ->
  Val.

-spec question_id(answer(), total_testing:id()) -> answer().
question_id(Doc, Val) ->
  maps:put(question_id, Val, Doc).

-spec description(answer()) -> total_testing:description().
description(#{description := Val}) ->
  Val.

-spec description(answer(), total_testing:description()) -> answer().
description(Doc, Val) ->
  maps:put(description, Val, Doc).

-spec is_correct(answer()) -> boolean().
is_correct(#{is_correct := Val}) ->
  Val.

-spec is_correct(answer(), boolean()) -> answer().
is_correct(Doc, Val) ->
  maps:put(is_correct, Val, Doc).
