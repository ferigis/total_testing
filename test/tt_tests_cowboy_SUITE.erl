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
%% @doc tests cowboy handler test suite.
%% @end
%%%-------------------------------------------------------------------
-module(tt_tests_cowboy_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common test
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% Tests
-export([common_case_test/1]).
-export([validate/1]).

-define(BASE_URL, "/rest/v1/tests/").
-define(HEADER_CT_APPJSON, #{<<"Content-Type">> => <<"application/json">>}).

%%%===================================================================
%%% Common Test
%%%===================================================================

all() ->
  [common_case_test, validate].

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

common_case_test(_Config) ->
  Test = tt_test:new(<<"Test 1">>),
  Body = tt_util:enc_json(Test),
  #{status_code := 200, body := Body1} = tt_test_util:api_call(post, ?BASE_URL,
    ?HEADER_CT_APPJSON, Body),
  #{<<"test_id">> := Id} = tt_util:dec_json(binary_to_list(Body1)),
  #{status_code := 404} = tt_test_util:api_call(get, ?BASE_URL ++ "fakeId"),
  #{status_code := 200, body := Body2} = tt_test_util:api_call(get, ?BASE_URL ++ Id),
  NewName = <<"This is a new name">>,
  Test1 = tt_test:from_map(tt_util:dec_json(binary_to_list(Body2))),
  Test2 = tt_test:name(Test1, NewName),
  Test3 = tt_util:enc_json(Test2),
  #{status_code := 204} = tt_test_util:api_call(put, ?BASE_URL ++ Id,
    ?HEADER_CT_APPJSON, Test3),
  #{status_code := 200, body := Body3} = tt_test_util:api_call(get, ?BASE_URL ++ Id),
  Test2 = tt_test:from_map(tt_util:dec_json(binary_to_list(Body3))),
  #{status_code := 204} = tt_test_util:api_call(delete, ?BASE_URL ++ Id),
  #{status_code := 404} = tt_test_util:api_call(get, ?BASE_URL ++ Id).

validate(_Config) ->
  BadTest = #{<<"dddd">> => <<"this is a name">>},
  Body = tt_util:enc_json(BadTest),
  #{status_code := 400, body := Body2} = tt_test_util:api_call(post, ?BASE_URL,
    ?HEADER_CT_APPJSON, Body),
  #{<<"error">> := #{
    <<"mandatory_query_param">> := <<"name">>
  }} = tt_util:dec_json(binary_to_list(Body2)).
