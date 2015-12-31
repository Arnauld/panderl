%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. déc. 2015 17:26
%%%-------------------------------------------------------------------
-module(city_srv).
-author("Arnauld").
-include("types.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2, stop/1]).
-export([
  increase_infection_level/3,
  change_infection_level/4,
  get_infection_levels/1,
  get_links/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3,
  format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {name,
  links = [],
  infection_levels = #{red=>0, black=>0, yellow=>0, blue=>0}}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(city(), links()) ->
  {ok, Pid :: pid()}
  | ignore
  | {error, Reason :: term()}).
start_link(City, Links)
  when ?is_city(City)
  andalso ?is_links(Links) ->
  gen_server:start_link({local, City}, ?MODULE, [City, Links], []).

%%--------------------------------------------------------------------
%% @doc
%% Stop the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(city()) -> ok).
stop(City) when ?is_city(City) ->
  gen_server:stop(City),
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Increase the infection level of the specified disease.
%%
%% Result of the infection is then (asynchronously) send to the originator provided,
%% referencing the returned reference.
%%
%% ```{infection_level_increased, City, Ref, Disease, NewLevel}
%%    {infection_level_increased, City, Ref, Disease, outbreak, Links}'''
%% @end
%%--------------------------------------------------------------------
-spec(increase_infection_level(city(), disease(), originator()) -> {ok, reference()}|city_not_started).
increase_infection_level(City, Disease, Originator)
  when ?is_city(City)
  andalso ?is_disease(Disease)
  andalso ?is_originator(Originator) ->
  Ref = make_ref(),
  % unfortunately `gen_server:cast` silently ignore if process is not alive
  cast_if_exists(City, {increase_infection_level, City, Disease, Originator, Ref}, Ref).

%%--------------------------------------------------------------------
%% @doc
%% Change the infection level of the specified disease.
%%
%% Result of the change is then (asynchronously) send to the originator provided,
%% referencing the returned reference.
%%
%% ```{infection_level_changed, City, Ref, Disease, NewLevel}'''
%% @end
%%--------------------------------------------------------------------
-spec(change_infection_level(city(), disease(), infection_level(), originator()) -> {ok, reference()}|city_not_started).
change_infection_level(City, Disease, NewLevel, Originator)
  when ?is_city(City)
  andalso ?is_disease(Disease)
  andalso ?is_originator(Originator) ->
  Ref = make_ref(),
  cast_if_exists(City, {change_infection_level, City, Disease, NewLevel, Originator, Ref}, Ref).


%%--------------------------------------------------------------------
%% @doc
%% Get the infection levels.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_infection_levels(City :: city()) -> {ok, {city(), infection_levels, infection_levels()}}).
get_infection_levels(City)
  when ?is_city(City) ->
  gen_server:call(City, {get_infection_levels, City}).

%%--------------------------------------------------------------------
%% @doc
%% Get the links (the cities linked to the provided one).
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_links(City :: city()) -> {ok, {city(), links, links()}}).
get_links(City)
  when ?is_city(City) ->
  gen_server:call(City, {get_links, City}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(init([city(), ...]) -> {ok, State :: #state{}}).
init([City, Links]) ->
  {ok, #state{name = City, links = Links}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}}).

handle_call({get_infection_levels, City}, _From, State) ->
  #state{name = City,
    infection_levels = Levels} = State,
  LevelsAsList = maps:to_list(Levels),
  {reply, {ok, {City, infection_levels, LevelsAsList}}, State};

handle_call({get_links, City}, _From, State) ->
  #state{name = City,
    links = Links} = State,
  {reply, {ok, {City, links, Links}}, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}}).

handle_cast({increase_infection_level, City, Disease, Originator, Ref}, State) ->
  #state{name = City,
    infection_levels = Levels,
    links = Links} = State,
  Level = maps:get(Disease, Levels, 0),
  case Level of
    3 ->
      Originator ! {infection_level_increased, City, Ref, Disease, outbreak, Links},
      {noreply, State};

    N when N < 3 ->
      NewLevel = Level + 1,
      NewLevels = maps:put(Disease, NewLevel, Levels),
      NewState = State#state{infection_levels = NewLevels},
      Originator ! {infection_level_increased, City, Ref, Disease, NewLevel},
      {noreply, NewState}
  end;

handle_cast({change_infection_level, City, Disease, NewLevel, Originator, Ref}, State) ->
  #state{name = City,
    infection_levels = Levels} = State,
  NewLevels = maps:put(Disease, NewLevel, Levels),
  NewState = State#state{infection_levels = NewLevels},
  Originator ! {infection_level_changed, City, Ref, Disease, NewLevel},
  {noreply, NewState};

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
format_status(_Opt, _StatusData) ->
  erlang:error(not_implemented).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(cast_if_exists(city(), term(), Ref :: reference()) -> {ok, Ref :: reference()}|city_not_started).
cast_if_exists(City, Msg, Ref) ->
  case whereis(City) of
    undefined ->
      city_not_started;
    Pid when is_pid(Pid) ->
      ok = gen_server:cast(City, Msg),
      {ok, Ref}
  end.

