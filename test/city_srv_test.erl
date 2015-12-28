%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. déc. 2015 18:21
%%%-------------------------------------------------------------------
-module(city_srv_test).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

should_provide_links__test() ->
  try
    city_srv:start_link(london, [paris, new_york]),
    {ok, {london, links, Links}} = city_srv:get_links(london),
    ?assertEqual([paris, new_york], Links)
  after
    city_srv:stop(london)
  end.

should_not_be_infected_by_default__test() ->
  try
    city_srv:start_link(london, [paris, new_york]),
    {ok, {london, infection_levels, Levels}} = city_srv:get_infection_levels(london),
    lists:foreach(fun(Disease) ->
      ?assertEqual(0, proplists:get_value(Disease, Levels, 0))
    end, [blue, red, black, yellow, pink])
  after
    city_srv:stop(london)
  end.

should_asynchronously_be_notified_with_new_infection_level_when_it_increases_test() ->
  try
    city_srv:start_link(london, [paris, new_york]),
    Ref1 = city_srv:increase_infection_level(london, blue, self()),
    Ref2 = city_srv:increase_infection_level(london, blue, self()),
    Ref3 = city_srv:increase_infection_level(london, red, self()),
    Ref4 = city_srv:increase_infection_level(london, yellow, self()),
    Ref5 = city_srv:increase_infection_level(london, blue, self()),
    Levels = lists:map(fun(Ref) ->
      receive
        {infection_level_increased, london, Ref, Disease, NewLevel} ->
          {Disease, NewLevel}
      end
    end, [Ref1, Ref2, Ref3, Ref4, Ref5]),
    ?assertEqual([1, 2, 3], proplists:get_all_values(blue, Levels)),
    ?assertEqual([1], proplists:get_all_values(red, Levels)),
    ?assertEqual([1], proplists:get_all_values(yellow, Levels))
  after
    city_srv:stop(london)
  end.

should_increase_infection_levels_and_then_get_corresponding_infection_levels__test() ->
  try
    city_srv:start_link(london, [paris, new_york]),
    city_srv:increase_infection_level(london, blue, self()),
    city_srv:increase_infection_level(london, blue, self()),
    city_srv:increase_infection_level(london, red, self()),
    {ok, {london, infection_levels, Levels}} = city_srv:get_infection_levels(london),
    ?assertEqual(2, proplists:get_value(blue, Levels)),
    ?assertEqual(1, proplists:get_value(red, Levels)),
    ?assertEqual(0, proplists:get_value(black, Levels)),
    ?assertEqual(0, proplists:get_value(yellow, Levels))
  after
    city_srv:stop(london)
  end.

should_asynchronously_be_notified_on_outbreak_test() ->
  try
    city_srv:start_link(london, [paris, new_york]),
    _Ref1 = city_srv:increase_infection_level(london, blue, self()),
    _Ref2 = city_srv:increase_infection_level(london, blue, self()),
    _Ref3 = city_srv:increase_infection_level(london, blue, self()),
    Ref4 = city_srv:increase_infection_level(london, blue, self()),
    %
    % selective receive, only care of the last Ref
    %
    Links = receive
              {infection_level_increased, london, Ref4, blue, outbreak, RLinks} ->
                RLinks
            after 1000 ->
              timeout
            end,
    ?assertEqual([paris, new_york], Links)
  after
    city_srv:stop(london)
  end.
