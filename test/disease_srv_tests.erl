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
-include("assert_ext.hrl").

should_be_initialized_with_24_cubes_by_default__test() ->
  try
    disease_srv:start_link(blue),
    {ok, {blue, nb_cubes, NbCubes}} = disease_srv:get_nb_cubes(blue),
    ?assertEqual(24, NbCubes)
  after
    disease_srv:stop(blue)
  end.

should_be_initialized_with_provided_nb_cubes__test() ->
  try
    disease_srv:start_link(blue, 32),
    {ok, {blue, nb_cubes, NbCubes}} = disease_srv:get_nb_cubes(blue),
    ?assertEqual(32, NbCubes)
  after
    disease_srv:stop(blue)
  end.

should_consume_cubes_for_a_particular_disease_and_be_notified_asynchronously__test() ->
  try
    disease_srv:start_link(blue, 42),

    {ok, Ref} = disease_srv:consume(blue, 2, self()),

    {ok, {blue, nb_cubes, NbCubes}} = disease_srv:get_nb_cubes(blue),
    ?assertEqual(40, NbCubes),
    receive
      {cube_consumed, Disease, Ref, NewNbCubes} ->
        ?assertEqual(blue, Disease),
        ?assertEqual(40, NewNbCubes)
    after 1000 ->
      ?fail(timeout)
    end
  after
    disease_srv:stop(blue)
  end.

should_consume_cubes_then_release_them_back_for_a_particular_disease_and_be_notified_asynchronously__test() ->
  try
    disease_srv:start_link(blue, 17),

    {ok, _Ref1} = disease_srv:consume(blue, 9, self()),
    {ok, Ref2} = disease_srv:release(blue, 3, self()),

    {ok, {blue, nb_cubes, NbCubes}} = disease_srv:get_nb_cubes(blue),
    ?assertEqual(11, NbCubes),
    receive
      {cube_released, Disease, Ref2, NewNbCubes} ->
        ?assertEqual(blue, Disease),
        ?assertEqual(11, NewNbCubes)
    after 1000 ->
      ?fail(timeout)
    end
  after
    disease_srv:stop(blue)
  end.

should_be_notified_when_there_is_not_enough_cube_to_consume__test() ->
  try
    disease_srv:start_link(blue, 7),

    {ok, Ref} = disease_srv:consume(blue, 12, self()),

    {ok, {blue, nb_cubes, NbCubes}} = disease_srv:get_nb_cubes(blue),
    ?assertEqual(0, NbCubes),
    receive
      {cube_consumed, Disease, Ref, not_enough_cubes} ->
        ?assertEqual(blue, Disease)
    after 1000 ->
      ?fail(timeout)
    end
  after
    disease_srv:stop(blue)
  end.
