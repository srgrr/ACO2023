-module(part2).
-export([main/0]).

diff_list(L) ->
  [lists:nth(I, L) - lists:nth(I - 1, L) || I <- lists:seq(2, length(L))].

% f - x = p
% x = f - p
process_list(L, Acc) ->
  case lists:all(fun _(X) -> X == 0 end, L) of
    true -> 0;
    _ ->
      DiffList = diff_list(L),
      DiffHead = process_list(DiffList, Acc),
      NewDiffHead = hd(L) - DiffHead,
      NewDiffHead
  end.

solve(Acc) ->
  case io:get_line("") of
    eof -> Acc;
    Line ->
      List = lists:map(
        fun _(X) -> {Val, _} = string:to_integer(X), Val end,
        string:split(string:replace(Line, "\n", "", all), " ", all)
      ),
      Result = process_list(List, 0),
      solve(Acc + Result)
  end.

main() ->
  R = solve(0),
  io:format("~p~n", [R]).
