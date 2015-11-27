%%%-------------------------------------------------------------------
%%% @author felipe
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Nov 2015 12:35 PM
%%%-------------------------------------------------------------------
-module(tt_test_util).
-author("felipe").

%% API
-export([api_call/2, api_call/4]).
-export([clear_backend/0]).


-spec api_call(atom(), string()) -> #{}.
api_call(Method, Uri) ->
  api_call(Method, Uri, #{}).

-spec api_call(atom(), string(), #{}) -> #{}.
api_call(Method, Uri, Headers) ->
  api_call(Method, Uri, Headers, []).

-spec api_call(atom(), string(), #{}, iodata()) -> #{}.
api_call(Method, Uri, Headers, Body) ->
  Port = application:get_env(example, http_port, 8082),
  {ok, Pid} = shotgun:open("localhost", Port),
  try
    {ok, Response} = shotgun:request(Pid, Method, Uri, Headers, Body, #{}),
    Response
  after
    shotgun:close(Pid)
  end.

-spec clear_backend() -> {aborted, term()} | {atomic, ok}.
clear_backend() ->
  mnesia:clear_table(test),
  mnesia:clear_table(question),
  mnesia:clear_table(answer).
