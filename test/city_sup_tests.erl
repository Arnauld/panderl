%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. déc. 2015 16:33
%%%-------------------------------------------------------------------
-module(city_sup_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").
-include("assert_ext.hrl").

should_start_cities__test() ->
  CityDefs = default_cyclic_linked_cities(),
  try
    city_sup:start_link(CityDefs),

    ?assertEqual([paris, madrid], get_links(london)),
    ?assertEqual([london, madrid], get_links(paris)),
    ?assertEqual([paris, london], get_links(madrid))
  after
    city_sup:stop()
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

default_cyclic_linked_cities() ->
  [
    {london, [paris, madrid]},
    {paris, [london, madrid]},
    {madrid, [paris, london]}].

get_links(City) ->
  {ok, {City, links, Links}} = city_srv:get_links(City),
  Links.

get_infection_level(City, Disease) ->
  {ok, {City, infection_levels, Levels}} = city_srv:get_infection_levels(City),
  proplists:get_value(Disease, Levels).
