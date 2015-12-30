%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. déc. 2015 09:57
%%%-------------------------------------------------------------------
-module(disease_sup).
-author("Arnauld").
-include("types.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1, stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() -> {ok, Pid :: pid()}).
start_link() ->
  start_link([blue, red, black, yellow]).

-spec(start_link([disease()|{disease(), non_neg_integer()}]) -> {ok, Pid :: pid()}).
start_link(DiseaseDefs) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [DiseaseDefs]).


-spec(stop() -> ok).
stop() ->
  gen_server:stop(?SERVER).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: pos_integer(), MaxT :: pos_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }}).
init([DiseaseDefs]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  Children = lists:map(fun(Def) ->
    case Def of
      {Disease, NbCubes} ->
        {Disease, {disease_srv, start_link, [Disease, NbCubes]},
          Restart, Shutdown, Type, [disease_srv]};
      Disease when ?is_disease(Disease) ->
        {Disease, {disease_srv, start_link, [Disease]},
          Restart, Shutdown, Type, [disease_srv]}
    end
  end, DiseaseDefs),

  {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
