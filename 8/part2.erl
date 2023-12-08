-module(part2).
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
  case lists:all(fun _(X) -> lists:last(X) == $Z end, Current) of
    true -> Acc;
    _ ->
      Next = [next_step(Mov, L, R) || [L, R] <- [maps:get(X, Graph) || X <- Current]],
      num_steps(T, M, Next, Graph, 1 + Acc)
  end.

solve() ->
  {Movements, _} = {io:get_line(""), io:get_line("")},
  Graph = read_graph(maps:new()),
  Origins = [X || X <- maps:keys(Graph), lists:last(X) == $A],
  num_steps(Movements, Movements, Origins, Graph, 0).

main() ->
  R = solve(),
  io:format("~p ~n", [R]).
