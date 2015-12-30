%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. déc. 2015 23:55
%%%-------------------------------------------------------------------
-module(disease_srv).
-author("Arnauld").
-include("types.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, stop/1]).

-export([
  consume/3,
  release/3,
  get_nb_cubes/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3,
  format_status/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_NB_CUBES, 24).

-record(state, {name :: disease(), remaining_cubes :: non_neg_integer()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(disease()) -> {ok, Pid :: pid()}).
start_link(Disease)
  when ?is_disease(Disease) ->
  start_link(Disease, ?DEFAULT_NB_CUBES).

-spec(start_link(disease(), non_neg_integer()) -> {ok, Pid :: pid()}).
start_link(Disease, NbCubes)
  when ?is_disease(Disease)
  andalso is_integer(NbCubes)
  andalso NbCubes >= 0 ->
  gen_server:start_link({local, Disease}, ?MODULE, [Disease, NbCubes], []).


%%--------------------------------------------------------------------
%% @doc
%% Stop the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(city()) -> ok).
stop(City) when ?is_city(City) ->
  gen_server:stop(City).

-spec(consume(disease(), pos_integer(), originator()) -> {ok, Ref :: reference()}).
consume(Disease, Amount, Originator) ->
  Ref = make_ref(),
  gen_server:cast(Disease, {consume, Disease, Amount, Originator, Ref}),
  {ok, Ref}.


%%--------------------------------------------------------------------
%% @doc
%% Release back nb cubes of the specified disease.
%%
%% Result of the change is then (asynchronously) send to the originator provided,
%% referencing the returned reference.
%%
%% ```{cube_released, Disease, Ref, NewNbCubes}'''
%% @end
%%--------------------------------------------------------------------
-spec(release(disease(), pos_integer(), originator()) -> {ok, Ref :: reference()}).
release(Disease, Amount, Originator)
  when ?is_disease(Disease)
  andalso is_integer(Amount)
  andalso ?is_originator(Originator) ->
  Ref = make_ref(),
  gen_server:cast(Disease, {release, Disease, Amount, Originator, Ref}),
  {ok, Ref}.


%%--------------------------------------------------------------------
%% @doc
%% Get the number of cubes remaining.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_nb_cubes(Disease :: disease()) -> {ok, {disease(), nb_cubes, non_neg_integer()}}).
get_nb_cubes(Disease)
  when ?is_disease(Disease) ->
  gen_server:call(Disease, {get_nb_cubes, Disease}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) -> {ok, State :: #state{}}).
init([Disease, NbCubes]) ->
  {ok, #state{name = Disease, remaining_cubes = NbCubes}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}}).
handle_call({get_nb_cubes, Disease}, _From, State) ->
  #state{name = Disease,
    remaining_cubes = NbCubes} = State,
  Reply = {ok, {Disease, nb_cubes, NbCubes}},
  {reply, Reply, State};

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
handle_cast({consume, Disease, Amount, Originator, Ref}, State) ->
  #state{name = Disease,
    remaining_cubes = NbCubes} = State,
  case NbCubes of
    _ when NbCubes >= Amount ->
      NewNbCubes = NbCubes - Amount,
      NewState = State#state{remaining_cubes = NewNbCubes},
      Originator ! {cube_consumed, Disease, Ref, NewNbCubes},
      {noreply, NewState};

    _ ->
      NewState = State#state{remaining_cubes = 0},
      Originator ! {cube_consumed, Disease, Ref, not_enough_cubes},
      {noreply, NewState}
  end;


handle_cast({release, Disease, Amount, Originator, Ref}, State) ->
  #state{name = Disease,
    remaining_cubes = NbCubes} = State,
  NewNbCubes = NbCubes + Amount,
  NewState = State#state{remaining_cubes = NewNbCubes},
  Originator ! {cube_released, Disease, Ref, NewNbCubes},
  {noreply, NewState};

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
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
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


format_status(_Opt, _StatusData) ->
  erlang:error(not_implemented).

%%%===================================================================
%%% Internal functions
%%%===================================================================
