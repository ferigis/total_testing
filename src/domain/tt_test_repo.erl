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
%% @doc Tests repository
%% @end
%%%-------------------------------------------------------------------
-module(tt_test_repo).

%% API
-export([init/0]).
-export([get/0, get/1]).
-export([set/1]).
-export([update/1]).
-export([delete/1]).

%%%===================================================================
%%% Types & Macros
%%%===================================================================

%% mnesia record
-record(test, {id :: total_testing:id(), 
							name :: total_testing:name()}).

%% types
-type test() :: #test{}.


%%====================================================================
%% API functions
%%====================================================================

%% @doc initialize the test table in mnesia
-spec init() -> ok.
init() ->
  mnesia:create_table(test,
                        [{attributes, record_info(fields, test)},
                        {disc_copies, [node()]}
                        ]).

%% @doc get all tests
-spec get() -> [tt_test:test()].
get() ->
  F = fun() -> mnesia:select(test,[{'_',[],['$_']}]) end,
  case mnesia:activity(transaction, F) of
    []      -> [];
    Result  ->
      lists:map(fun record_to_test/1, Result)
  end.

%% @doc get test by id
-spec get(total_testing:id()) -> tt_test:test() | not_found.
get(TestId) ->
	case mnesia:dirty_read(test, TestId) of
    [] -> not_found;
    [Test] ->
      record_to_test(Test)
  end.

%% @doc create a new test
-spec set(tt_test:test()) -> {ok, total_testing:id()}.
set(Test) when is_map(Test) ->
	save(Test).

%% @doc update a test
-spec update(tt_test:test()) -> ok.
update(Test) when is_map(Test) ->
	{ok, _} = save(Test),
	ok.

%% @doc delete a test by id
-spec delete(total_testing:id()) -> ok.
delete(TestId) ->
	F = fun() -> 
    mnesia:delete({test, TestId})
  end,
  mnesia:activity(transaction, F),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
-spec test_to_record(tt_test:test()) -> test().
test_to_record(#{id := Id, name := Name}) ->
	#test{id = Id, name = Name}.

%% @private
-spec record_to_test(test()) -> tt_test:test().
record_to_test(Test) when is_record(Test, test) ->
	Test2 = tt_test:new(Test#test.id, Test#test.name),
  tt_test:questions_number(Test2, length(tt_question_repo:get_by_test(Test#test.id))).

%% @private
-spec save(tt_test:test()) -> {ok, total_testing:id()}.
save(Test) when is_map(Test) ->
	TestRecord = test_to_record(Test),
	F = fun() ->
    mnesia:write(test, TestRecord, write)
  end,
  mnesia:activity(transaction, F),
  {ok, TestRecord#test.id}.
