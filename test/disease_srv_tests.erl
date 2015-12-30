%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. déc. 2015 00:45
%%%-------------------------------------------------------------------
-module(disease_srv_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

should_be_initialized_with_24_cubes_by_default__test() ->
  try
    disease_srv:start_link(blue),
    {ok, {blue, nb_cubes, NbCubes}} = disease_srv:get_nb_cubes(blue),
    ?assertEqual(24, NbCubes)
  after
    disease_srv:stop(blue)
  end.
