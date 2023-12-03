-module(part2).
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
  Coords = {I + RI, J + RJ},
  CurrentSet = maps:get(Coords, Acc, sets:new()),
  NewSet = sets:add_element({I, J}, CurrentSet),
  add_neighbor_list(T, I, J, maps:put(Coords, NewSet, Acc)).

add_neighbors_to_marked(I, J, Acc) ->
  Neighbors = get_neighbor_offsets(),
  add_neighbor_list(Neighbors, I, J, Acc).

is_digit(X) -> X >= $0 andalso X =< $9.

process_row_for_marked(_, _, [C], Acc) when C == $\n -> Acc;
process_row_for_marked(RowId, ColId, [Cell | T], Acc) ->
  case (Cell == $*) of
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

add_number_to_origins(_, [], _, Acc) -> Acc;
add_number_to_origins(CurrentNumber, [Origin | T], MarkedToOrigins, Acc) ->
  add_number_to_origins(
    CurrentNumber,
    T,
    MarkedToOrigins,
    maps:put(Origin, sets:add_element(CurrentNumber, maps:get(Origin, Acc, sets:new())), Acc)
  ).

add_origins_for_cell(I, J, Marked, Detected) ->
  sets:union(
    Detected,
    maps:get({I, J}, Marked, sets:new())
  ).

process_numbers_in_row(_, _, [C], MarkedToOrigin, DetectedOrigins, CurrentNumber, Acc)
  when C == $\n ->
    add_number_to_origins(CurrentNumber, sets:to_list(DetectedOrigins), MarkedToOrigin, Acc);
process_numbers_in_row(
  RowId,
  ColId,
  [Cell | T],
  MarkedToOrigin,
  DetectedOrigins,
  CurrentNumber,
  Acc) ->
    case is_digit(Cell) of
      true ->
        process_numbers_in_row(
          RowId, ColId + 1, T, MarkedToOrigin,
          add_origins_for_cell(RowId, ColId, MarkedToOrigin, DetectedOrigins),
          10 * CurrentNumber + Cell - $0,
          Acc
        );
      _ ->
        process_numbers_in_row(
          RowId, ColId + 1, T, MarkedToOrigin, sets:new(), 0,
          add_number_to_origins(CurrentNumber, sets:to_list(DetectedOrigins), MarkedToOrigin, Acc)
        )
    end.

process_row(_, [], _, Acc) -> Acc;
process_row(RowId, [Row | T], MarkedToOrigin, Acc) ->
  process_row(
    RowId + 1,
    T,
    MarkedToOrigin,
    process_numbers_in_row(RowId, 1, Row, MarkedToOrigin, sets:new(), 0, Acc)
  ).


get_asterisk_to_number_list(InputMatrix, MarkedToOrigin) ->
  process_row(1, InputMatrix, MarkedToOrigin, maps:new()).


compute_answer(AsteriskToNumberList) ->
  MapLists = lists:map(fun _(X) -> sets:to_list(X) end, maps:values(AsteriskToNumberList)),
  ListsWithTwo = lists:filter(fun _(X) -> length(X) == 2 end, MapLists),
  Products = lists:map(fun _([A, B]) -> A * B end, ListsWithTwo),
  lists:foldl(fun _(A, B) -> A + B end, 0, Products).

main() ->
  InputMatrix = lists:reverse(read_input_matrix([])),
  MarkedToOrigin = get_marked_indices(1, InputMatrix, maps:new()),
  AsteriskToNumberList = get_asterisk_to_number_list(InputMatrix, MarkedToOrigin),
  Answer = compute_answer(AsteriskToNumberList),
  Answer.

solve() ->
  Sum = main(),
  io:format("~w~n", [Sum]).
