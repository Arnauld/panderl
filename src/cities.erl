%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. déc. 2015 22:00
%%%-------------------------------------------------------------------
-module(cities).
-author("Arnauld").
-include("types.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
  city_defs_from_raw_links/1,
  default_raw_links/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec(city_defs_from_raw_links([{city(), city()}]) -> [city_def()]).
city_defs_from_raw_links(Links) ->
  CitySet = lists:foldl(fun({City1, City2}, Multimap) ->
    L1 = maps:get(City1, Multimap, []),
    L2 = maps:get(City2, Multimap, []),
    M1 = maps:put(City1, [City2 | L1], Multimap),
    M2 = maps:put(City2, [City1 | L2], M1),
    M2
  end, #{}, Links),
  maps:to_list(CitySet).

-spec(default_raw_links() -> [{city(), city()}]).
default_raw_links() ->
  [
% Connect all blues
    {san_francisco, chicago},
    {chicago, atlanta},
    {chicago, toronto},
    {atlanta, washington},
    {toronto, washington},
    {toronto, new_york},
    {washington, new_york},
    {new_york, madrid},
    {new_york, london},
    {madrid, london},
    {madrid, paris},
    {london, paris},
    {london, essen},
    {paris, essen},
    {paris, milan},
    {essen, milan},
    {essen, saint_petersburg},

% Connect all blacks
    {algiers, paris},
    {algiers, madrid},
    {algiers, istanbul},
    {algiers, cairo},
    {istanbul, milan},
    {istanbul, cairo},
    {istanbul, baghdad},
    {istanbul, moscow},
    {istanbul, saint_petersburg},
    {cairo, baghdad},
    {cairo, riyadh},
    {moscow, saint_petersburg},
    {baghdad, riyadh},
    {baghdad, karachi},
    {baghdad, tehran},
    {tehran, moscow},
    {riyadh, karachi},
    {tehran, karachi},
    {tehran, delhi},
    {karachi, delhi},
    {karachi, mumbai},
    {delhi, kolkata},
    {delhi, mumbai},
    {mumbai, chennai},
    {kolkata, chennai},

% Connect all reds
    {jakarta, bangkok},
    {jakarta, ho_chi_minh},
    {jakarta, chennai},
    {jakarta, sydney},
    {sydney, manila},
    {bangkok, chennai},
    {bangkok, kolkata},
    {bangkok, ho_chi_minh},
    {bangkok, hong_kong},
    {ho_chi_minh, manila},
    {ho_chi_minh, hong_kong},
    {manila, san_francisco},
    {manila, hong_kong},
    {manila, taipei},
    {hong_kong, taipei},
    {hong_kong, shanghai},
    {hong_kong, kolkata},
    {taipei, osaka},
    {taipei, shanghai},
    {osaka, tokyo},
    {shanghai, beijing},
    {shanghai, seoul},
    {shanghai, tokyo},
    {beijing, seoul},
    {seoul, tokyo},
    {tokyo, san_francisco},

% Connect all yellows
    {los_angeles, sydney},
    {los_angeles, mexico_city},
    {los_angeles, san_francisco},
    {los_angeles, chicago},
    {mexico_city, miami},
    {mexico_city, chicago},
    {mexico_city, bogota},
    {mexico_city, lima},
    {lima, bogota},
    {santiago, lima},
    {bogota, miami},
    {bogota, sao_paulo},
    {bogota, buenos_aires},
    {miami, atlanta},
    {miami, washington},
    {buenos_aires, sao_paulo},
    {sao_paulo, madrid},
    {sao_paulo, lagos},
    {lagos, khartoum},
    {lagos, kinshasa},
    {kinshasa, khartoum},
    {kinshasa, johannesburg},
    {johannesburg, khartoum},
    {khartoum, cairo}].