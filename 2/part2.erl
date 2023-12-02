-module(part2).
-export([solve/0]).

get_game_map([], Acc) -> Acc;
get_game_map([NumColor | T], Acc) ->
  [BinaryNum, Color] = string:split(NumColor, " ", all),
  Num = list_to_integer(binary_to_list(BinaryNum)),
  get_game_map(T, maps:put(binary_to_list(Color), Num, Acc)).

maximap([], _, _, Acc) -> Acc;
maximap([Key | T], A, B, Acc) ->
  M = maps:put(
      Key,
      erlang:max(maps:get(Key, A, 0), maps:get(Key, B, 0)),
      Acc
    ),
  maximap(T, A, B, M).

mapmaxi(A, B) ->
  maximap(["red", "green", "blue"], A, B, maps:new()).

process_game(GameList) ->
  ListOfGames = string:split(GameList, "; ", all),
  MaxiMap =
    lists:foldl(
      fun _(A, B) -> mapmaxi(A, B) end,
      maps:new(),
      lists:map(
        fun _(X) -> get_game_map(string:split(X, ", ", all), maps:new()) end,
        ListOfGames
      )
    ),
  lists:foldl(
    fun _(A, B) -> A * B end,
    1,
    maps:values(MaxiMap)
  ).

main(Accm) ->
  case io:get_line("") of
    eof -> Accm;
    Line ->
      {_, [_, GameList]} = re:run(Line, "Game (\\d+): (.*)", [{capture, all_but_first, binary}]),
      main(Accm + process_game(GameList))
  end.

solve() ->
  R = main(0),
  io:format("~w~n", [R]).
