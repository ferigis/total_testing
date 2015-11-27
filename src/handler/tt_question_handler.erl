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
%%% @doc  cowboy questions handler
%%% @end
%%% Created : 10. Nov 2015 2:27 AM
%%%-------------------------------------------------------------------
-module(tt_question_handler).
-author("Felipe Ripoll <ferigis@gmail.com>").

%% API
%% Cowboy behavior
-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([forbidden/2]).
-export([resource_exists/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([delete_resource/2]).


%% handlers
-export([handle_post/2]).
-export([handle_put/2]).
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
  {[<<"POST">>, <<"GET">>, <<"DELETE">>, <<"PUT">>], Req, State}.

%% @hidden
forbidden(Req, State) ->
  {Method, _} = cowboy_req:method(Req),
  {Id, _} = cowboy_req:binding(id, Req),
  case {Method, Id} of
    {<<"POST">>, _} ->
      {false, Req, State#{val => null}};
    {<<"GET">>, undefined} ->
      {false, Req, State#{val => null}};
    {_, undefined} ->
      {true, Req, State#{val => null}};
    {_, _} ->
      case total_testing:get_question(Id) of
        not_found ->
          {false, Req, State#{val => null}};
        #{id := Id} = Question ->
          {false, Req, State#{val => Question}}
      end
  end.

%% @hidden
resource_exists(Req, #{val := null} = State) ->
  {Method, _} = cowboy_req:method(Req),
  {Id, _} = cowboy_req:binding(id, Req),
  case {Method, Id} of
    {<<"POST">>, _} ->
      {false, Req, State};
    {<<"GET">>, undefined} ->
      {true, Req, State};
    {_,_} ->
      {false, Req, State}
  end;
resource_exists(Req, State) ->
  {true, Req, State}.

%% @hidden
content_types_accepted(Req, State) ->
  Function = case cowboy_req:method(Req) of
               {<<"POST">>, _}  -> handle_post;
               {<<"PUT">>, _}   -> handle_put
             end,
  ContentTypes = [{{<<"application">>, <<"json">>, '*'}, Function}],
  {ContentTypes, Req, State}.

%% @hidden
content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_get}], Req, State}.

%% @hidden
handle_post(Req, State) ->
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    DecQuestion = tt_util:dec_json(Body),
    Question = tt_question:from_map(DecQuestion),
    ok = validate_question(Question),
    {ok, Id} = total_testing:create_question(Question),
    Body2 = tt_util:enc_json(#{<<"question_id">> => Id}),
    Req2 = cowboy_req:set_resp_body(Body2, Req1),
    {true, Req2, State}
  catch
    throw:{missing_param, Param} ->
      tt_util:handle_exception({missing_query_param, Param}, Req, State);
    _:_Ex ->
      %% log and check if is bad_json or not
      tt_util:handle_exception(bad_json, Req, State)
  end.

%% @hidden
handle_put(Req, #{val := null} = State) ->
  tt_util:handle_exception(notfound, Req, State);
handle_put(Req, #{val := Question} = State) ->
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    DecQuestion = tt_util:dec_json(Body),
    Question1 = tt_question:from_map(DecQuestion),
    Question2 = tt_question:id(Question1, tt_question:id(Question)),
    ok = validate_question(Question2),
    ok = total_testing:update_question(Question2),
    {true, Req1, State}
  catch
    throw:{missing_param, Param} ->
      tt_util:handle_exception({missing_query_param, Param}, Req, State);
    _:_Ex ->
      %% log and check if is bad_json or not
      tt_util:handle_exception(bad_json, Req, State)
  end.

%% @hidden
handle_get(Req, #{val := Question} = State) ->
  Body = tt_util:enc_json(Question),
  {Body, Req, State}.

%% @hidden
delete_resource(Req, #{val := Question} = State) ->
  try
    ok = total_testing:delete_question(tt_question:id(Question)),
    {true, Req, State}
  catch
    _:Ex ->
      tt_util:handle_exception(Ex, Req, State)
  end.


%%====================================================================
%% Internal functions
%%====================================================================

-spec validate_question(tt_question:question()) -> ok.
validate_question(Question) ->
  case tt_question:description(Question) of
    null ->
      throw({missing_param, <<"description">>});
    _ -> ok
  end,
  case tt_question:answers(Question) of
    [] ->
      throw({missing_param, <<"answers">>});
    Answers ->
      lists:map(fun validate_answer/1, Answers),
      ok
  end.

-spec validate_answer(tt_question:answer()) -> ok.
validate_answer(Answer) ->
  case tt_question:description(Answer) of
    null ->
      throw({missing_param, <<"answer/description">>});
    _ -> ok
  end.
