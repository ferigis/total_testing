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
%%%-------------------------------------------------------------------
%%% @author Felipe Ripoll <ferigis@gmail.com>
%%% @copyright (C) 2015, <Felipe Ripoll>, All Rights Reserved.
%%% @doc  cowboy test questions handler
%%% @end
%%% Created : 10. Nov 2015 2:27 AM
%%%-------------------------------------------------------------------
-module(tt_test_questions_handler).
-author("Felipe Ripoll <ferigis@gmail.com>").

%% API
%% Cowboy behavior
-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([forbidden/2]).
-export([resource_exists/2]).
-export([content_types_provided/2]).

%% handlers
-export([handle_get/2]).

%%%===================================================================
%%% Cowboy callbacks
%%%===================================================================

%% @hidden
init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

%% @hidden
rest_init(Req, _Opts) ->
  {ok, Req, #{}}.

%% @hidden
allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

%% @hidden
forbidden(Req, State) ->
  {Id, _} = cowboy_req:binding(id, Req),
  case Id of
    undefined ->
      {true, Req, State#{val => null}};
    Id ->
      case total_testing:get_questions_by_test(Id) of
        [] ->
          {false, Req, State#{val => []}};
        Questions ->
          {false, Req, State#{val => Questions}}
      end
  end.

%% @hidden
resource_exists(Req, State) ->
  {true, Req, State}.

%% @hidden
content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_get}], Req, State}.

%% @hidden
handle_get(Req, #{val := Questions} = State) ->
  Body = tt_util:enc_json(Questions),
  {Body, Req, State}.
