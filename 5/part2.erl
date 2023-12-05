-module(part2).
-export([main/0]).

get_seed_intervals([], Acc) -> Acc;
get_seed_intervals([Ls, Ds | T], Acc) ->
  [L, D] = lists:map(fun _(X) -> {Val, _} = string:to_integer(X), Val end, [Ls, Ds]),
  get_seed_intervals(T, [[L, L + D - 1, notfound] | Acc]).

get_seed_list() ->
  [SeedLine, _] = [io:get_line(""), io:get_line("")],
  {_, [SeedNumsBin]} =
    re:run(SeedLine, "seeds: (.*)\n", [{capture, all_but_first, binary}]),
  get_seed_intervals(
    string:split(binary_to_list(SeedNumsBin), " ", all),
    []
  ).

add_map(Acc, CurrentMap) ->
  case io:get_line("") of
    eof -> Acc ++ [CurrentMap];
    "\n" -> Acc ++ [CurrentMap];
    Line ->
      {_, Matches} =
        re:run(Line, "(\\d+) (\\d+) (\\d+)\n", [{capture, all_but_first, binary}]),
      [Dest, Source, Len] =
        lists:map(fun _(X) -> {Val, _} = string:to_integer(X), Val end, Matches),
      add_map(Acc, CurrentMap ++ [[Source, Source + Len - 1, Dest]])
  end.
    

get_map_list(Acc) ->
  case io:get_line("") of
    eof -> Acc;
    _ -> get_map_list(add_map(Acc, []))
  end.

add_nonempty_rems([], Acc) -> Acc;

add_nonempty_rems([[L, R, _] = I | T], Acc) when L =< R ->
  add_nonempty_rems(T, [I | Acc]);

add_nonempty_rems([_ | T], Acc) ->
  add_nonempty_rems(T, Acc).


replace_intervals([] , _, Acc) -> Acc;


replace_intervals([[SL, SR, _] = Seed | T1], [L, R, _] = Interval, Acc)
  when (SR < L orelse SL > R) ->
  replace_intervals(T1, Interval, [Seed | Acc]);

replace_intervals([[_, _, found] = Seed | T1], Interval, Acc) ->
  replace_intervals(T1, Interval, [Seed | Acc]);

replace_intervals([[SL, SR, notfound] | T1], [L, R, D] = Interval, Acc) ->
  [IL, IR] = [erlang:max(L, SL), erlang:min(R, SR)],
  NewInterval = [D + IL - L, D + IR - L, found],
  LeftRemainder = [SL, IL - 1, notfound],
  RightRemainder = [IR + 1, SR, notfound],
  ToAdd = add_nonempty_rems([LeftRemainder, RightRemainder], []),
  replace_intervals(
    ToAdd ++ T1,
    Interval,
    [NewInterval | Acc]
  ).

get_next_intervals(CurrentIntervals, []) ->
  [[X, Y, notfound] || [X, Y, _] <- CurrentIntervals];
get_next_intervals(CurrentIntervals, [MapInterval | T]) ->
  get_next_intervals(
    replace_intervals(CurrentIntervals, MapInterval, []),
    T
  ).

iterate_seed(CurrentIntervals, []) -> CurrentIntervals;
iterate_seed(CurrentIntervals, [Map | T]) ->
  iterate_seed(get_next_intervals(CurrentIntervals, Map), T).

solve() ->
  Seeds = get_seed_list(),
  Maps = get_map_list([]),
  Locations =
  lists:foldl(
    fun _(A, B) -> A ++ B end,
    [],
    lists:map(
      fun _(X) -> iterate_seed([X], Maps) end,
      Seeds
    )
  ),
  [MinVal, _, _] = lists:foldl(
    fun _([A, _, _], [B, _, _]) -> [erlang:min(A, B), 3, xd] end,
    [2934892358988132, 3, xd],
    Locations
  ),
  MinVal.

main() ->
  R = solve(),
  io:format("~w~n", [R]).
