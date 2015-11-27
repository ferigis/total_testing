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
%% @doc questions cowboy handler test suite.
%% @end
%%%-------------------------------------------------------------------
-module(tt_questions_cowboy_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common test
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% Tests
-export([common_case/1]).
-export([validate/1]).

-define(TESTS_URL, "/rest/v1/tests/").
-define(BASE_URL, "/rest/v1/questions/").
-define(HEADER_CT_APPJSON, #{<<"Content-Type">> => <<"application/json">>}).

%%%===================================================================
%%% Common Test
%%%===================================================================

all() ->
  [common_case, validate].

init_per_suite(Config) ->
  shotgun:start(),
  total_testing:start(),
  Config.

end_per_suite(Config) ->
  shotgun:stop(),
  tt_test_util:clear_backend(),
  ok = application:stop(total_testing),
  Config.

%%%===================================================================
%%% Exported Tests Functions
%%%===================================================================

common_case(_Config) ->
  Test = tt_test:new(<<"Test 1">>),
  Body = tt_util:enc_json(Test),
  #{status_code := 200, body := Body1} = tt_test_util:api_call(post, ?TESTS_URL,
    ?HEADER_CT_APPJSON, Body),
  #{<<"test_id">> := TestId} = tt_util:dec_json(binary_to_list(Body1)),
  Answer1 = tt_answer:new(null, <<"Answer 1 Description">>, false),
  Answer2 = tt_answer:new(null, <<"Answer 2 Description">>, true),
  Question = tt_question:new(TestId, <<"Question 1">>, [Answer1, Answer2]),
  Body2 = tt_util:enc_json(Question),
  #{status_code := 200, body := Body3} = tt_test_util:api_call(post, ?BASE_URL,
    ?HEADER_CT_APPJSON, Body2),
  #{<<"question_id">> := QuestionId} = tt_util:dec_json(binary_to_list(Body3)),
  #{status_code := 404} = tt_test_util:api_call(get, ?BASE_URL ++ "fakeId"),
  #{status_code := 200, body := Body4} = tt_test_util:api_call(get, ?BASE_URL ++ QuestionId),
  Question1 = tt_question:from_map(tt_util:dec_json(binary_to_list(Body4))),
  2 = length(tt_question:answers(Question1)),
  ModifiedDescription = <<"This is a new description">>,
  Question2 = tt_question:description(Question1, ModifiedDescription),
  Question3 = tt_question:answers(Question2, [Answer1]),
  Question4 = tt_util:enc_json(Question3),
  #{status_code := 204} = tt_test_util:api_call(put, ?BASE_URL ++ QuestionId,
    ?HEADER_CT_APPJSON, Question4),
  #{status_code := 200, body := Body5} = tt_test_util:api_call(get, ?BASE_URL ++ QuestionId),
  Question5 = tt_question:from_map(tt_util:dec_json(binary_to_list(Body5))),
  1 = length(tt_question:answers(Question5)),
  #{status_code := 204} = tt_test_util:api_call(delete, ?BASE_URL ++ QuestionId),
  #{status_code := 404} = tt_test_util:api_call(get, ?BASE_URL ++ QuestionId).

validate(_Config) ->
  Test = tt_test:new(<<"Test 1">>),
  Body = tt_util:enc_json(Test),
  #{status_code := 200, body := Body1} = tt_test_util:api_call(post, ?TESTS_URL,
    ?HEADER_CT_APPJSON, Body),
  #{<<"test_id">> := TestId} = tt_util:dec_json(binary_to_list(Body1)),
  Question = tt_question:new(TestId, <<"Question 1">>, []),
  Body2 = tt_util:enc_json(Question),
  #{status_code := 400, body := Body3} = tt_test_util:api_call(post, ?BASE_URL,
    ?HEADER_CT_APPJSON, Body2),
  #{<<"error">> := #{
    <<"mandatory_query_param">> := <<"answers">>
  }} = tt_util:dec_json(binary_to_list(Body3)),
  BadAnswer = #{<<"dddd">> => <<"this is a description">>},
  Question2 = tt_question:new(TestId, <<"Question 1">>, [BadAnswer]),
  Body4 = tt_util:enc_json(Question2),
  #{status_code := 400, body := Body5} = tt_test_util:api_call(post, ?BASE_URL,
    ?HEADER_CT_APPJSON, Body4),
  #{<<"error">> := #{
    <<"mandatory_query_param">> := <<"answer/description">>
  }} = tt_util:dec_json(binary_to_list(Body5)).
