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
%% @doc test model.
%% @end
%%%-------------------------------------------------------------------
-module(tt_test).

%% API
-export([new/1, new/2]).
-export([from_map/1]).
-export([id/1, id/2]).
-export([name/1, name/2]).
-export([questions_number/1, questions_number/2]).

%%%===================================================================
%%% Types & Macros
%%%===================================================================

-type test() :: #{id                => total_testing:id(),
                  name              => total_testing:name(),
                  questions_number  => integer()}.

%% exported types
-export_types([test/0]).

%%====================================================================
%% API functions
%%====================================================================

-spec new(total_testing:name()) -> test().
new(Name) ->
  new(null, Name).

-spec new(total_testing:id(), total_testing:name()) -> test().
new(Id, Name) ->
  #{id => Id, name => Name}.

-spec from_map(map()) -> test().
from_map(Map) ->
  #{
    id    => maps:get(<<"id">>, Map, null),
    name  => maps:get(<<"name">>, Map, null)
  }.

-spec id(test()) -> total_testing:id().
id(#{id := Val}) ->
  Val.

-spec id(test(), total_testing:id()) -> test().
id(Doc, Val) ->
  maps:put(id, Val, Doc).

-spec name(test()) -> total_testing:name().
name(#{name := Val}) ->
  Val.

-spec name(test(), total_testing:name()) -> test().
name(Doc, Val) ->
  maps:put(name, Val, Doc).

-spec questions_number(test()) -> integer().
questions_number(#{questions_number := Val}) ->
  Val.

-spec questions_number(test(), integer()) -> test().
questions_number(Doc, Val) ->
  maps:put(questions_number, Val, Doc).
