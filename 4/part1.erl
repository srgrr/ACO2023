-module(part1).
-export([solve/0]).

score_of(0) -> 0;
score_of(1) -> 1;
score_of(N) -> 2 * score_of(N - 1).

process_line(Line) ->
  [_, CardInfo] = string:split(string:replace(Line, "  ", " ", all), ":"),
  [Winning, Rest] = 
    lists:map(
      fun _(X) -> string:split(string:trim(X), " ", all) end,
      string:split(CardInfo, "|")
    ),
  score_of(sets:size(
    sets:intersection(
      sets:from_list(Winning),
      sets:from_list(Rest)
    )
  )).

main(Acc) ->
  case io:get_line("") of
    eof -> Acc;
    Line -> main(process_line(Line) + Acc)
  end.

solve() ->
  R = main(0),
  io:format("~p~n", [R]).
