-module(part1).
-export([main/0]).

process_input_row(Acc, [C], _, _) when C == $\n -> Acc;
process_input_row(Acc, [H | T], RowId, ColId) ->
  process_input_row(maps:put({RowId, ColId}, H, Acc), T, RowId, ColId + 1).

read_input_matrix(Acc, RowId) ->
  case io:get_line("") of
    eof -> Acc;
    Line -> read_input_matrix(process_input_row(Acc, Line, RowId, 0), RowId + 1)
  end.


get_points_row(_, _, _, _, MaxCol, _, ColId, Acc) when ColId > MaxCol -> Acc;
get_points_row(Matrix, EmptyRows, EmptyCols, MaxRow, MaxCol, RowId, ColId, Acc) ->
  case maps:get({RowId, ColId}, Matrix) of
    $# ->
      I = RowId + length([X || X <- EmptyRows, X < RowId]),
      J = ColId + length([X || X <- EmptyCols, X < ColId]),
      NewMap = sets:add_element({I, J}, Acc),
      get_points_row(Matrix, EmptyRows, EmptyCols, MaxRow, MaxCol, RowId, ColId + 1, NewMap);
    _ ->
      get_points_row(Matrix, EmptyRows, EmptyCols, MaxRow, MaxCol, RowId, ColId + 1, Acc)
  end.
  

get_points(_, _, _, MaxRow, _, RowId, Acc) when RowId > MaxRow -> Acc;
get_points(Matrix, EmptyRows, EmptyCols, MaxRow, MaxCol, RowId, Acc) ->
  get_points(Matrix, EmptyRows, EmptyCols, MaxRow, MaxCol, RowId + 1,
    get_points_row(Matrix, EmptyRows, EmptyCols, MaxRow, MaxCol, RowId, 0, Acc)
  ).

manhattan({Ai, Aj}, {Bi, Bj}) -> abs(Ai - Bi) + abs(Aj - Bj).

solve() ->
  InputMatrix = read_input_matrix(maps:new(), 0),
  {MaxRow, MaxCol} = lists:foldl(fun erlang:max/2, {-1, -1}, maps:keys(InputMatrix)),
  EmptyRows =
    [I || I <- lists:seq(0, MaxRow),
                lists:all(fun _(J) -> maps:get({I, J}, InputMatrix) == $. end, lists:seq(0, MaxCol))],
  EmptyCols =
    [J || J <- lists:seq(0, MaxCol),
                lists:all(fun _(I) -> maps:get({I, J}, InputMatrix) == $. end, lists:seq(0, MaxRow))],
  io:format("~p ~p~n", [EmptyRows, EmptyCols]),
  Points = sets:to_list(get_points(InputMatrix, EmptyRows, EmptyCols, MaxRow, MaxCol, 0, sets:new())),
  AllDists =
    [manhattan(P, Q) || P <- Points, Q <- Points, P < Q],
  lists:foldl(fun _(A, B) -> A + B end, 0, AllDists).

main() ->
  R = solve(),
  io:format("~p~n", [R]).
