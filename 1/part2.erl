-module(part2).
-export([solve/0]).

digit_map() ->
  #{
    "one" => 1,
    "two" => 2,
    "three" => 3,
    "four" => 4,
    "five" => 5,
    "six" => 6,
    "seven" => 7,
    "eight" => 8,
    "nine" => 9,
    "1" => 1,
    "2" => 2,
    "3" => 3,
    "4" => 4,
    "5" => 5,
    "6" => 6,
    "7" => 7,
    "8" => 8,
    "9" => 9
  }.

starts_with(_, []) -> true;
starts_with([], _) -> false;
starts_with([H1 | T1], [H2 | T2]) ->
  case H1 =:= H2 of
    true -> starts_with(T1, T2);
    _ -> false
  end.

is_prefix(Line) ->
  P = lists:filter(
    fun _(X) -> starts_with(Line, X) end,
    maps:keys(digit_map())
  ),
  case P of
    [] -> false;
    L  -> {found, hd(L)}
  end.

remove_prefix(Line, []) -> Line;
remove_prefix(Line, [H]) when (H == $o orelse H == $t orelse H == $e) -> Line;
remove_prefix([_|T1], [_|T2]) -> remove_prefix(T1, T2).

digitize([], Accm) -> Accm;
digitize([_ | T] = Line, Accm) ->
  case is_prefix(Line) of
    {found, Prefix} ->
      digitize(remove_prefix(Line, Prefix), [maps:get(Prefix, digit_map()) | Accm]);
    _ ->
      digitize(T, Accm)
  end.

process_line(Line) ->
  D = lists:reverse(digitize(Line, [])),
  [First, Last] = [hd(D), lists:last(D)],
  io:format("~p = ~w~n",[Line, 10 * First + Last]),
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
    lists:reverse(read_all_lines([]))
  ),
  io:format("~w~n",[R]).
