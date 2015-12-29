%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. déc. 2015 00:36
%%%-------------------------------------------------------------------
-module(infestor_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

should_infect_at_least_target_city__test() ->
  Defs = default_cyclic_linked_cities(),
  try
    start_cities(Defs),

    {ok, Journal} = infestor:infect(london, blue),

    ?assertEqual([{propagate, london}, {infected, london, 1}], Journal),
    ?assertEqual(1, get_infection_level(london, blue)),
    % Other cities remain unchanged
    ?assertEqual(0, get_infection_level(madrid, blue)),
    ?assertEqual(0, get_infection_level(paris, blue))
  after
    stop_cities(Defs)
  end.

should_infect_and_propagate_to_linked_cities_on_outbreak__test() ->
  Defs = default_cyclic_linked_cities(),
  try
    start_cities(Defs),
    city_srv:change_infection_level(london, blue, 3, self()),

    {ok, Journal} = infestor:infect(london, blue),

    ?assertEqual([{propagate, london},
      {outbreak, london},
      {propagate, paris},
      {propagate, madrid},
      {infected, paris, 1},
      {infected, madrid, 1}], Journal),

    ?assertEqual(3, get_infection_level(london, blue)),
    ?assertEqual(1, get_infection_level(madrid, blue)),
    ?assertEqual(1, get_infection_level(paris, blue))
  after
    stop_cities(Defs)
  end.


should_infect_and_chain_outbreak__test() ->
  Defs = default_cyclic_linked_cities(),
  try
    start_cities(Defs),
    city_srv:change_infection_level(london, blue, 3, self()),
    city_srv:change_infection_level(paris, blue, 3, self()),
    city_srv:change_infection_level(madrid, blue, 2, self()),

    {ok, Journal} = infestor:infect(london, blue),

    ?assertEqual([{propagate, london},
      {outbreak, london},
      {propagate, paris},
      {propagate, madrid},
      {outbreak, paris},
      {propagate, madrid},
      {infected, madrid, 3},
      {outbreak, madrid}], Journal),

    ?assertEqual(3, get_infection_level(london, blue)),
    ?assertEqual(3, get_infection_level(madrid, blue)),
    ?assertEqual(3, get_infection_level(paris, blue))
  after
    stop_cities(Defs)
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_infection_level(City, Disease) ->
  {ok, {City, infection_levels, Levels}} = city_srv:get_infection_levels(City),
  proplists:get_value(Disease, Levels).

default_cyclic_linked_cities() ->
  [
    {london, [paris, madrid]},
    {paris, [london, madrid]},
    {madrid, [paris, london]}].

stop_cities(Defs) ->
  lists:foreach(fun({City, _Links}) ->
    city_srv:stop(City)
  end, Defs).


start_cities(Defs) ->
  lists:foreach(fun({City, Links}) ->
    city_srv:start_link(City, Links)
  end, Defs).