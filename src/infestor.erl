%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. déc. 2015 23:28
%%%-------------------------------------------------------------------
-module(infestor).
-author("Arnauld").

-define(TIMEOUT, 5000).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([infect/2]).

-export([propagate/6]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
infect(City, Disease) ->
  Originator = self(),
  ToInfect = [City],
  AlreadyOutbreaked = sets:new(),
  Pending = [],
  Journal = [],
  _Pid = spawn_link(?MODULE, propagate, [Originator, ToInfect, Disease, AlreadyOutbreaked, Pending, Journal]),
  receive
    {ok, JournalUpdated} ->
      {ok, lists:reverse(JournalUpdated)}

  after ?TIMEOUT ->
    timeout
  end.


propagate(Originator, [], _Disease, _AlreadyOutbreaked, [], Journal) ->
  Originator ! {ok, Journal};

propagate(Originator, [ToInfect | OthersToInfect], Disease, AlreadyOutbreaked, Pending, Journal) ->
  % Only propagate if not already outbreaked
  case sets:is_element(ToInfect, AlreadyOutbreaked) of
    true ->
      propagate(Originator, OthersToInfect, Disease, AlreadyOutbreaked, Pending, Journal);
    false ->
      {ok, Ref} = city_srv:increase_infection_level(ToInfect, Disease, self()),
      NewJournal = [{propagate, ToInfect} | Journal],
      propagate(Originator, OthersToInfect, Disease, AlreadyOutbreaked, [{ToInfect, Ref} | Pending], NewJournal)
  end;

propagate(Originator, [], Disease, AlreadyOutbreaked, Pending, Journal) ->
  receive
    {infection_level_increased, City, Ref, Disease, NewLevel} ->
      NewJournal = [{infected, City, NewLevel} | Journal],
      NewPending = lists:delete({City, Ref}, Pending),
      propagate(Originator, [], Disease, AlreadyOutbreaked, NewPending, NewJournal);

    {infection_level_increased, City, Ref, Disease, outbreak, Links} ->
      Outbreaked = sets:add_element(City, AlreadyOutbreaked),
      NewJournal = [{outbreak, City} | Journal],
      NewPending = lists:delete({City, Ref}, Pending),
      propagate(Originator, Links, Disease, Outbreaked, NewPending, NewJournal)
  end.
