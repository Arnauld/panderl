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

-define(TIMEOUT_NOTIF, 1000).
-define(TIMEOUT_INFECTION, 5000).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([infect/2]).

-export([propagate/7]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
infect(City, Disease) ->
  Originator = self(),
  ToInfect = [City],
  AlreadyOutbreaked = sets:new(),
  Pending = [],
  Journal = [],
  Ref = make_ref(),
  _Pid = spawn_link(?MODULE, propagate, [Originator, Ref, ToInfect, Disease, AlreadyOutbreaked, Pending, Journal]),
  receive
    {ok, Ref, JournalUpdated} ->
      {ok, lists:reverse(JournalUpdated)};

    {error, Ref, Reason} ->
      {error, Reason}

  after ?TIMEOUT_INFECTION ->
    timeout
  end.


propagate(Originator, Ref, [], _Disease, _AlreadyOutbreaked, [], Journal) ->
  Originator ! {ok, Ref, Journal};

propagate(Originator, Ref, [ToInfect | OthersToInfect], Disease, AlreadyOutbreaked, Pending, Journal) ->
  % Only propagate if not already outbreaked
  case sets:is_element(ToInfect, AlreadyOutbreaked) of
    true ->
      propagate(Originator, Ref, OthersToInfect, Disease, AlreadyOutbreaked, Pending, Journal);
    false ->
      case city_srv:increase_infection_level(ToInfect, Disease, self()) of
        {ok, InfectRef} ->
          NewJournal = [{propagate, ToInfect} | Journal],
          NewPending = [{ToInfect, InfectRef} | Pending],
          propagate(Originator, Ref, OthersToInfect, Disease, AlreadyOutbreaked, NewPending, NewJournal);
        city_not_started ->
          NewJournal = [{propagation_failure, ToInfect, city_not_started} | Journal],
          propagate(Originator, Ref, OthersToInfect, Disease, AlreadyOutbreaked, Pending, NewJournal)
      end
  end;

propagate(Originator, Ref, [], Disease, AlreadyOutbreaked, Pending, Journal) ->
  receive
    {infection_level_increased, City, InfectRef, Disease, NewLevel} ->
      NewJournal = [{infected, City, NewLevel} | Journal],
      NewPending = lists:delete({City, InfectRef}, Pending),
      propagate(Originator, Ref, [], Disease, AlreadyOutbreaked, NewPending, NewJournal);

    {infection_level_increased, City, InfectRef, Disease, outbreak, Links} ->
      Outbreaked = sets:add_element(City, AlreadyOutbreaked),
      NewJournal = [{outbreak, City} | Journal],
      NewPending = lists:delete({City, InfectRef}, Pending),
      propagate(Originator, Ref, Links, Disease, Outbreaked, NewPending, NewJournal);

    Other ->
      NewJournal = [{unrecognized_message, Other} | Journal],
      propagate(Originator, Ref, [], Disease, AlreadyOutbreaked, Pending, NewJournal)

  after ?TIMEOUT_NOTIF ->
    error_logger:warning_msg("infestor:propagate:3 - No message received but some are still pending ~n~p~n", [Pending]),
    Originator ! {error, Ref, {timeout, {journal, Journal}, {pending, Pending}}}
  end.
