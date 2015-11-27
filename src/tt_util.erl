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
%% @doc total testing util module.
%% @end
%%%-------------------------------------------------------------------
-module(tt_util).

%% Api
-export([generate_id/0]).
-export([enc_json/1]).
-export([dec_json/1]).
-export([handle_exception/3]).

%%====================================================================
%% API
%%====================================================================

%% @doc generates a unique id
-spec generate_id() -> binary().
generate_id() ->
  <<I:128/integer>> = crypto:hash(md5, term_to_binary({make_ref(), erlang:timestamp()})),
  erlang:integer_to_binary(I, 16).

-spec enc_json(any()) -> iodata().
enc_json(Json) ->
  jiffy:encode(Json, [uescape]).

-spec dec_json(iodata()) -> any().
dec_json(Json) ->
  try jiffy:decode(Json, [return_maps])
  catch
    _:{error, _} ->
      throw(bad_json)
  end.

handle_exception(bad_request, Req, State) ->
  {false, Req, State};
handle_exception(bad_type, Req, State) ->
  {false, Req, State};
handle_exception(badarg, Req, State) ->
  {false, Req, State};
handle_exception(bad_key, Req, State) ->
  {false, Req, State};
handle_exception(bad_json, Req, State) ->
  Req1 = set_resp_error(bad_json, Req),
  {false, Req1, State};
handle_exception({missing_query_param, Param}, Req, State) ->
  Req1 = set_resp_error(#{mandatory_query_param => Param}, Req),
  {false, Req1, State};
handle_exception({missing_field, Field}, Req, State) ->
  Req1 = set_resp_error(#{missing_field => Field}, Req),
  {false, Req1, State};
handle_exception(notfound, Req, State) ->
  Req1 = cowboy_req:reply(404, Req),
  {halt, Req1, State};
handle_exception(_Reason, Req, State) ->
  Req1 =
    try cowboy_req:reply(500, Req)
    catch
      _:_ ->
        {ok, Req}
    end,
  {halt, Req1, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
set_resp_error(Error, Req) ->
  ResBody = enc_json(#{error => Error}),
  cowboy_req:set_resp_body(ResBody, Req).
