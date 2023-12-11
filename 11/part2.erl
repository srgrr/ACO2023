-module(part2).
-export([main/0]).

process_input_row(Acc, [C], _, ColId) when C == $\n -> {Acc, ColId};
process_input_row(Acc, [H | T], RowId, ColId) ->
  NewAcc =
    case H == $# of
      true -> [{RowId, ColId} | Acc];
      _ -> Acc
    end,
  process_input_row(NewAcc, T, RowId, ColId + 1).

read_input_matrix(Acc, RowId) ->
  case io:get_line("") of
    eof -> {Acc, RowId, 0};
    Line ->
      {RowList, NumCols} = process_input_row(Acc, Line, RowId, 0),
      {FinalList, NumRows, _} = read_input_matrix(RowList, RowId + 1),
      {FinalList, NumRows, NumCols}
  end.

get_shifted_points(Matrix, EmptyRows, EmptyCols) ->
  [
    {
      I + 999999 * length([X || X <- EmptyRows, I > X]),
      J + 999999 * length([X || X <- EmptyCols, J > X])}
      || {I, J} <- Matrix
  ].

solve() ->
  {InputMatrix, NumRows, NumCols} = read_input_matrix([], 0),
  MatrixSet = sets:from_list(InputMatrix),
  Contained = fun _(C) -> sets:is_element(C, MatrixSet) end,
  EmptyRows =
    [I || I <- lists:seq(0, NumRows - 1),
     not lists:any(fun _(J) -> Contained({I, J}) end, lists:seq(0, NumCols - 1))],
  EmptyCols =
    [J || J <- lists:seq(0, NumCols - 1),
      not lists:any(fun _(I) -> Contained({I, J}) end, lists:seq(0, NumRows - 1))],
  Points = get_shifted_points(InputMatrix, EmptyRows, EmptyCols),
  Manhattan = fun _({Ai, Aj}, {Bi, Bj}) -> abs(Ai - Bi) + abs(Aj - Bj) end,
  AllDists = [Manhattan(P, Q) || P <- Points, Q <- Points, P < Q],
  lists:foldl(fun _(A, B) -> A + B end, 0, AllDists).

main() ->
  R = solve(),
  io:format("~p~n", [R]).
