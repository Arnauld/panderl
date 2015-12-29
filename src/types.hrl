%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. déc. 2015 17:31
%%%-------------------------------------------------------------------
-author("Arnauld").

-type city() :: atom().
-type disease() :: blue|yellow|black|red.
-type links() :: [city()].
-type city_def() :: {city(), links()}.

-type infection_level() :: 0|1|2|3.
-type infection_result() :: infection_level()|outbreak.
-type infection_levels() :: [{disease(), infection_level()}].

-type originator() :: pid()|atom().

-define(is_city(City), is_atom(City)).
-define(is_links(Links), is_list(Links)).
-define(is_disease(Disease), is_atom(Disease)).
-define(is_originator(Originator), (is_pid(Originator) or is_atom(Originator))).
