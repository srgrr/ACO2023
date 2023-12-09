-module(part1).
-export([main/0]).

diff_list(L) ->
  [lists:nth(I, L) - lists:nth(I - 1, L) || I <- lists:seq(2, length(L))].

process_list(L, Acc) ->
  io:format("~p~n", [L]),
  case lists:all(fun _(X) -> X == 0 end, L) of
    true -> Acc;
    _ -> process_list(diff_list(L), Acc + lists:last(L))
  end.

solve(Acc) ->
  case io:get_line("") of
    eof -> Acc;
    Line ->
      List = lists:map(
        fun _(X) -> {Val, _} = string:to_integer(X), Val end,
        string:split(string:replace(Line, "\n", "", all), " ", all)
      ),
      solve(Acc + process_list(List, 0))
  end.

main() ->
  R = solve(0),
  io:format("~p~n", [R]).
