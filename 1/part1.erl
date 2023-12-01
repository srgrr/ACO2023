-module(part1).
-export([solve/0]).

get_digits(Line) ->
  lists:map(
    fun _(X) -> X - $0 end,
    lists:filter(
      fun _(X) -> X >= $0 andalso X =< $9 end,
      Line
    )
  ).

process_line(Line) ->
    L = get_digits(Line),
    [First, Last] = [hd(L), lists:last(L)],
    10 * First + Last.

solve_for_input(Lines) ->
  lists:foldl(
    fun _(A, B) -> A + B end,
    0,
    lists:map(
      fun _(Line) -> process_line(Line) end,
      Lines
    )
  ).

read_all_lines(Acc) ->
  case io:get_line("") of
    eof -> Acc;
    Line -> read_all_lines([Line | Acc])
  end.

solve() ->
  R = solve_for_input(
    read_all_lines([])
  ),
  io:format("~w~n",[R]).
