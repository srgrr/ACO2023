-module(part1).
-export([main/0]).

sample() -> {[7, 15, 30], [9, 40, 200]}.
input() -> {[63, 78, 94, 68], [411, 1274, 2047, 1035]}.

num_ways([], [], Acc) -> Acc;
num_ways([Time | T1], [Distance | T2], Acc) ->
  NumWays =
    length([X || X <- lists:seq(0, Time), X * (Time - X) > Distance]),
  io:format("For case ~p ~p there are ~p ways ~n", [Time, Distance, NumWays]),
  num_ways(T1, T2, Acc * NumWays).

solve() ->
  {Times, Distances} = input(),
  num_ways(Times, Distances, 1).

main() ->
  R = solve(),
  io:format("~p~n", [R]).
