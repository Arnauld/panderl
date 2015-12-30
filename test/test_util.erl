%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. déc. 2015 15:53
%%%-------------------------------------------------------------------
-module(test_util).
-author("Arnauld").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([drain_all_messages/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec(drain_all_messages() -> [Msgs :: term()]).
drain_all_messages() ->
  drain_all_messages([]).

drain_all_messages(Msgs) ->
  receive
    X ->
      drain_all_messages([X | Msgs])
  after 0 ->
    Msgs
  end.