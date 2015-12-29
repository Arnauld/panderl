%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. déc. 2015 12:56
%%%-------------------------------------------------------------------
-author("Arnauld").

-ifdef(NOASSERT).
-define(fail(Msg), ok).
-else.
%% The assert macro is written the way it is so as not to cause warnings
%% for clauses that cannot match, even if the expression is a constant.
-define(fail(Msg),
  begin
    ((fun() ->
      erlang:error({assert,
        [{module, ?MODULE},
          {line, ?LINE},
          {expression, (??Msg)}]})
    end)())
  end).
-endif.
