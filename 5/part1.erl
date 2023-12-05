-module(part1).
-export([main/0]).

get_seed_list() ->
  [SeedLine, _] = [io:get_line(""), io:get_line("")],
  {_, [SeedNumsBin]} =
    re:run(SeedLine, "seeds: (.*)\n", [{capture, all_but_first, binary}]),
  lists:map(
    fun _(X) -> {Val, _} = string:to_integer(X), Val end,
    string:split(binary_to_list(SeedNumsBin), " ", all)
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

get_next_id(CurrentId, []) -> CurrentId;
get_next_id(CurrentId, [[L, R, D] | _]) when (CurrentId >= L andalso CurrentId =< R) ->
  D + CurrentId - L;
get_next_id(CurrentId, [_ | T]) ->
  get_next_id(CurrentId, T).

iterate_seed(CurrentId, []) -> CurrentId;
iterate_seed(CurrentId, [Map | T]) ->
  NextId = get_next_id(CurrentId, Map),
  iterate_seed(NextId, T).


solve() ->
  Seeds = get_seed_list(),
  Maps = get_map_list([]),
  lists:foldl(
    fun _(A, B) -> erlang:min(A, B) end,
    23898895723587,
    lists:map(
      fun _(X) -> iterate_seed(X, Maps) end,
      Seeds
    )
  ).

main() ->
  R = solve(),
  io:format("~p~n", [R]).
