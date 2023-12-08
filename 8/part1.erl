-module(part1).
-export([main/0]).

read_graph(Acc) ->
  case io:get_line("") of
    eof -> Acc;
    Line ->
      {_, [NodeBin, LeftBin, RightBin]} =
        re:run(Line, "(.*) = \\((.*), (.*)\\)\n", [{capture, all_but_first, binary}]),
      [Node, Left, Right] = lists:map(fun binary_to_list/1, [NodeBin, LeftBin, RightBin]),
      NewGraph = maps:put(Node, [Left, Right], Acc),
      read_graph(NewGraph)
  end.

next_step(Mov, L, _) when Mov == $L -> L;
next_step(_, _, R) -> R.

num_steps([Mov], M, Current, Graph, Acc) when Mov == $\n ->
  num_steps(M, M, Current, Graph, Acc);

num_steps([Mov | T], M, Current, Graph, Acc) ->
  case Current == "ZZZ" of
    true -> Acc;
    _ ->
      [L, R] = maps:get(Current, Graph),
      Next = next_step(Mov, L, R),
      num_steps(T, M, Next, Graph, 1 + Acc)
  end.

solve() ->
  {Movements, _} = {io:get_line(""), io:get_line("")},
  Graph = read_graph(maps:new()),
  num_steps(Movements, Movements, "AAA", Graph, 0).

main() ->
  R = solve(),
  io:format("~p ~n", [R]).
