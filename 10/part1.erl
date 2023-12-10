-module(part1).
-export([main/0]).

get_row_map(_, _, [L], Acc) when L == $\n -> Acc;
get_row_map(RowId, ColId, [H | T], Acc) ->
  get_row_map(RowId, ColId + 1, T, maps:put({RowId, ColId}, H, Acc)).

read_graph(RowId, Acc) ->
  case io:get_line("") of
    eof -> Acc;
    Line ->
      RowMap = get_row_map(RowId, 0, Line, Acc),
      read_graph(RowId + 1, RowMap)
  end.

get_pipe_directions(PipeType) ->
  case PipeType of
    $| -> [[-1, 0], [1, 0]];
    $- -> [[0, -1], [0, 1]];
    $L -> [[-1, 0], [0, 1]];
    $J -> [[-1, 0], [0, -1]];
    $7 -> [[1, 0], [0, -1]];
    $F -> [[1, 0], [0, 1]];
    _ -> []
  end.

neighbors_are_compatible(Origin, Neighbor, Graph) ->
  NeighborNeighbors = sets:from_list(get_neighbors(Neighbor, maps:get(Neighbor, Graph, "?"))),
  sets:is_element(Origin, NeighborNeighbors).

get_neighbors({I, J}, PipeType) ->
  [{I + RI, J + RJ} || [RI, RJ] <- get_pipe_directions(PipeType)].

longest_loop(Current, Start, _, First, _, Acc) when Current == Start andalso not First -> Acc;
longest_loop(Current, Start, Parent, _, Graph, Acc) ->
  PipeType = maps:get(Current, Graph),
  NextNeighbor = hd([X || X <- get_neighbors(Current, PipeType), X /= Parent]),
  case neighbors_are_compatible(Current, NextNeighbor, Graph) of
    false -> 0;
    _ -> longest_loop(NextNeighbor, Start, Current, false, Graph, 1 + Acc)
  end.
  

get_pipe_types() -> "|-LJ7F".

solve() ->
  Graph = read_graph(0, maps:new()),
  StartingPosition =
    hd(lists:filter(fun _(Key) -> maps:get(Key, Graph) == $S end, maps:keys(Graph))),
  Answer =
    lists:foldl(
      fun erlang:max/2, 0,
      [longest_loop(StartingPosition, StartingPosition, {-1, -1}, true, maps:put(StartingPosition, PipeType, Graph), 0)
      || PipeType <- get_pipe_types()]
    ),
  Answer.

main() ->
  R = solve(),
  io:format("~p~n", [(R + 1) div 2]).
