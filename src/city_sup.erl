%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. déc. 2015 15:54
%%%-------------------------------------------------------------------
-module(city_sup).
-author("Arnauld").
-include("types.hrl").

-behaviour(supervisor).

%% API
-export([start_link/1, stop/0]).

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
-spec(start_link(CityDefs :: [city_def()]) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(CityDefs) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [CityDefs]).

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
  {ok,
    {SupFlags :: {supervisor:strategy(), MaxR :: pos_integer(), MaxT :: pos_integer()},
      [ChildSpec :: supervisor:child_spec()]
    }}).
init([CityDefs]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,
  Children = lists:map(fun({City, Links}) ->
    {City, {city_srv, start_link, [City, Links]},
      Restart, Shutdown, Type, ['city_srv']}
  end, CityDefs),

  {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
