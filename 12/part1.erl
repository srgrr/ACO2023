-module(part1).
-export([main/0]).

get_next_number_list_wall([H | T]) when H < 0 -> get_next_number_list_wall([-H | T]);
get_next_number_list_wall([H | T]) ->
  case H > 0 of
    true -> [H - 1 | T];
    _ -> T
  end.

get_next_number_list_dot([H | T] = List) ->
  case H == 0 of
    true -> T;
    _ -> List
  end.

wall_is_suitable([]) -> false;
wall_is_suitable([H | _]) -> H /= 0.

dot_is_suitable([]) -> true;
dot_is_suitable([H | _]) -> H =< 0.


calc([], [], StateMap) -> {1, StateMap};

calc([], NumberList, StateMap) ->
  Val =
    case NumberList == [0] of
      true -> 1;
      _ -> 0
    end,
  {Val, StateMap};

calc(Pattern, [], StateMap) ->
  Val = case lists:all(fun _(X) -> X /= $# end, Pattern) of
    true -> 1;
    _ -> 0
  end,
  {Val, StateMap};

calc([P | T1] = Pattern, NumberList, StateMap) ->
  Key = {Pattern, NumberList},
  NextNumberListForWall = get_next_number_list_wall(NumberList),
  NextNumberListForDot = get_next_number_list_dot(NumberList),
  case maps:is_key(Key, StateMap) of
    true -> {maps:get(Key, StateMap), StateMap};
    _ ->
      case P of
        $? ->
          {ValWith, StateMapWith} =
            case wall_is_suitable(NumberList) of
              true -> calc(T1, NextNumberListForWall, StateMap);
              _ -> {0, StateMap}
            end,
          {ValWithout, StateMapWithout} =
            case dot_is_suitable(NumberList) of
              true -> calc(T1, NextNumberListForDot, StateMapWith);
              _ -> {0, StateMapWith}
            end,
          TotalVal = ValWith + ValWithout,
          {TotalVal, maps:put(Key, TotalVal, StateMapWithout)};
        $# ->
          {Val, NewStateMap} =
            case wall_is_suitable(NumberList) of
              true -> calc(T1, NextNumberListForWall, StateMap);
              _ -> {0, StateMap}
            end,
          {Val, maps:put(Key, Val, NewStateMap)};
        $. ->
          {Val, NewStateMap} =
            case dot_is_suitable(NumberList) of
              true -> calc(T1, NextNumberListForDot, StateMap);
              _ -> {0, StateMap}
            end,
          {Val, maps:put(Key, Val, NewStateMap)}
      end
  end.

compute(Pattern, NumberList) -> calc(Pattern, NumberList, maps:new()).

solve(Acc) ->
  case io:get_line("") of
    eof -> Acc;
    Line ->
      [Pattern, NumberListStr] =
        string:split(string:replace(Line, "\n", ""), " ", all),
      NumberList =
        lists:map(
          fun _(X) -> {Val, _} = string:to_integer(X), -Val end,
          string:split(NumberListStr, ",", all)
        ),
      ReplicatedPattern =
        string:join([Pattern || _ <- lists:seq(1, 5)], "?"),
      ReplicatedNumberList = lists:flatten([NumberList || _ <- lists:seq(1, 5)]),
      {Val, _} = compute(ReplicatedPattern, ReplicatedNumberList),
      solve(Acc + Val)
  end.

main() ->
  R = solve(0),
  io:format("~p~n", [R]).
