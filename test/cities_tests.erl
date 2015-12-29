%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. déc. 2015 23:31
%%%-------------------------------------------------------------------
-module(cities_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

should_generate_default_layout__test() ->
  RawLinks = cities:default_raw_links(),
  CityDefs = cities:city_defs_from_raw_links(RawLinks),
  assertSameValues([london, algiers, madrid, essen, milan], paris, CityDefs),
  assertSameValues([new_york, madrid, paris, essen], london, CityDefs),
  assertSameValues([miami, washington, chicago], atlanta, CityDefs).

%%%===================================================================
%%% Internal functions
%%%===================================================================

assertSameValues(Values, City, CityDefs) ->
  ?assertEqual(
    lists:sort(Values),
    lists:sort(proplists:get_value(City, CityDefs))).
