%%%-------------------------------------------------------------------
%% @doc total_testing app
%% @end
%%%-------------------------------------------------------------------
-module(tt_app).

-behaviour(application).

%% Application callbacks
-export([start/0,
        start/2,
        stop/1]).
-export([start_phase/3]).

%%====================================================================
%% API
%%====================================================================

-spec start() -> {ok, _} | {error, term()}.
start() ->
  application:ensure_all_started(total_testing).


start(_StartType, _StartArgs) ->
  tt_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

start_phase(start_cowboy_listeners, _StartType, []) ->
  Routes     = cowboy_routes(),
  Dispatch   = cowboy_router:compile(Routes),
  ProtoOpts  = [{env, [{dispatch, Dispatch}]}],
  TransOpts  = [{port, 8082}],
  Cacceptors = 10,
  cowboy:start_http(http, Cacceptors, TransOpts, ProtoOpts),
  ok;
start_phase(start_datastore, _StartType, []) ->
  ok = start_store().


%%====================================================================
%% Internal functions
%%====================================================================

%% @private
cowboy_routes() ->
  [
    {'_',
      [
        {"/", cowboy_static, {priv_file, total_testing, "index.html"}},
        {"/assets/[...]", cowboy_static, {priv_dir, total_testing, "assets"}},
        %% Rest API
        {"/rest/v1/tests/[:id]", tt_test_handler, []},
        {"/rest/v1/tests/[:id]/questions", tt_test_questions_handler, []},
        {"/rest/v1/questions/[:id]", tt_question_handler, []},
        {"/rest/v1/questions/random/[:number]", tt_random_questions_handler, []}
      ]
    }
  ].

%% @private
start_store() ->
  Nodes = [node()],
  application:stop(mnesia),
  mnesia:create_schema(Nodes),
  application:start(mnesia),
  tt_test_repo:init(),
  tt_question_repo:init(),
  tt_answer_repo:init(),
  ok.
