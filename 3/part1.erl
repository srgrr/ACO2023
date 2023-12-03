-module(part1).
-export([solve/0]).

read_input_matrix(Acc) ->
  case io:get_line("") of
    eof -> Acc;
    Line -> read_input_matrix([string:strip(Line) | Acc])
  end.

get_neighbor_offsets() ->
  [{X, Y} || X <- lists:seq(-1, 1), Y <- lists:seq(-1, 1), X /= 0 orelse Y /= 0].

add_neighbor_list([], _, _, Acc) -> Acc;
add_neighbor_list([{RI, RJ} | T], I, J, Acc) ->
  add_neighbor_list(T, I, J, sets:add_element({I + RI, J + RJ}, Acc)).

add_neighbors_to_marked(I, J, Acc) ->
  Neighbors = get_neighbor_offsets(),
  add_neighbor_list(Neighbors, I, J, Acc).

is_digit(X) -> X >= $0 andalso X =< $9.

process_row_for_marked(_, _, [C], Acc) when C == $\n -> Acc;
process_row_for_marked(RowId, ColId, [Cell | T], Acc) ->
  case (Cell /= $. andalso not is_digit(Cell)) of
    true ->
      process_row_for_marked(
        RowId,
        ColId + 1,
        T,
        add_neighbors_to_marked(RowId, ColId, Acc)
      );
    _ -> process_row_for_marked(RowId, ColId + 1, T, Acc)
  end.

get_marked_indices(_, [], Acc) -> Acc;
get_marked_indices(RowId, [Row | T], Acc) ->
  get_marked_indices(RowId + 1, T, process_row_for_marked(RowId, 1, Row, Acc)).

add_if_needed(MustAdd, CurrentNumber, Acc) ->
  case MustAdd of
    true -> CurrentNumber + Acc;
    _ -> Acc
  end.

get_row_numbers_sum(_, _, [C], _, MustAdd, CurrentNumber, Acc) when C == $\n ->
  add_if_needed(MustAdd, CurrentNumber, Acc);
get_row_numbers_sum(RowId, ColId, [Cell | T], Marked, MustAdd, CurrentNumber, Acc) ->
  case is_digit(Cell) of
    true ->
      get_row_numbers_sum(
        RowId,
        ColId + 1,
        T,
        Marked,
        MustAdd or sets:is_element({RowId, ColId}, Marked),
        10 * CurrentNumber + Cell - $0,
        Acc);
    _ ->
      get_row_numbers_sum(
        RowId,
        ColId + 1,
        T,
        Marked,
        false,
        0,
        add_if_needed(MustAdd, CurrentNumber, Acc)
      )
  end.

get_marked_numbers_sum(_, [], _, Acc) -> Acc;
get_marked_numbers_sum(RowId, [Row | T], Marked, Acc) ->
  get_marked_numbers_sum(
    RowId + 1,
    T,
    Marked,
    get_row_numbers_sum(RowId, 1, Row, Marked, false, 0, Acc)
  ).

main() ->
  InputMatrix = lists:reverse(read_input_matrix([])),
  MarkedIndices = get_marked_indices(1, InputMatrix, sets:new()),
  Sum = get_marked_numbers_sum(1, InputMatrix, MarkedIndices, 0),
  Sum.


solve() ->
  Sum = main(),
  io:format("~w~n", [Sum]).
