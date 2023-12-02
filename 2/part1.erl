-module(part1).
-export([solve/0]).

get_game_map([], Acc) -> Acc;
get_game_map([NumColor | T], Acc) ->
  [BinaryNum, Color] = string:split(NumColor, " ", all),
  Num = list_to_integer(binary_to_list(BinaryNum)),
  get_game_map(T, maps:put(binary_to_list(Color), Num, Acc)).

% only 12 red cubes, 13 green cubes, and 14 blue cubes?
color_limits() -> #{"red" => 12, "green" => 13, "blue" => 14}.

valid(Game) ->
  ColorLimits = color_limits(),
  GameCount = get_game_map(string:split(Game, ", ", all), maps:new()),
  lists:all(
    fun _(Key) -> maps:get(Key, GameCount, 0) =< maps:get(Key, ColorLimits) end,
    ["red", "green", "blue"]
  ).

process_game(GameId, GameList) ->
  ListOfGames = string:split(GameList, "; ", all),
  AllGamesAreFine =
    lists:all(
      fun _(X) -> valid(X) end,
      ListOfGames
    ),
  case AllGamesAreFine of
    true -> GameId;
    _ -> 0
  end.

main(Accm) ->
  case io:get_line("") of
    eof -> Accm;
    Line ->
      {_, [GameId, GameList]} = re:run(Line, "Game (\\d+): (.*)", [{capture, all_but_first, binary}]),
      main(Accm + process_game(list_to_integer(binary_to_list(GameId)), GameList))
  end.

solve() ->
  R = main(0),
  io:format("~w~n", [R]).
