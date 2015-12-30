%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. déc. 2015 13:15
%%%-------------------------------------------------------------------
-module(disease_sup_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

should_start_the_4_classic_diseases_by_default__test() ->
  disease_sup:start_link(),
  try

    ?assertEqual({ok, {black, nb_cubes, 24}}, disease_srv:get_nb_cubes(black)),
    ?assertEqual({ok, {blue, nb_cubes, 24}}, disease_srv:get_nb_cubes(blue)),
    ?assertEqual({ok, {red, nb_cubes, 24}}, disease_srv:get_nb_cubes(red)),
    ?assertEqual({ok, {yellow, nb_cubes, 24}}, disease_srv:get_nb_cubes(yellow))

  after
    disease_sup:stop()
  end.

should_allow_custom_settings_for_each_disease__test() ->
  disease_sup:start_link([{pink, 16}, {blue, 9}]),
  try

    ?assertEqual({ok, {pink, nb_cubes, 16}}, disease_srv:get_nb_cubes(pink)),
    ?assertEqual({ok, {blue, nb_cubes, 9}}, disease_srv:get_nb_cubes(blue)),

    ?assert(is_pid(whereis(pink))),

    % other diseases should not be started
    ?assertEqual(undefined, whereis(red)),
    ?assertEqual(undefined, whereis(black)),
    ?assertEqual(undefined, whereis(yellow))

  after
    disease_sup:stop()
  end.
