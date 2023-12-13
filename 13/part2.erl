-module(part2).
-export([main/0]).

-record(grid, {map, rows, cols}).

get_current_sizes(Map) ->
  lists:foldl(fun erlang:max/2, {0, 0}, maps:keys(Map)).

compose_grid(Map) ->
  {MaxRow, MaxCol} = get_current_sizes(Map),
  #grid{
    map=Map,
    rows=MaxRow,
    cols=MaxCol
  }.

transpose(Grid) ->
  TransposedCellMap = maps:from_list([{{J, I}, C} || {{I, J}, C} <- maps:to_list(Grid#grid.map)]),
  #grid{
    map=TransposedCellMap,
    rows=Grid#grid.cols,
    cols=Grid#grid.rows
  }.

read_grid(Acc) ->
  case io:get_line("") of
    eof -> null;
    "\n" -> compose_grid(Acc);
    Line ->
      [TLine] = string:tokens(Line, "\n"),
      {CurrentRow, _} = get_current_sizes(Acc),
      AddCellFun = fun _({C, J}, M) -> maps:put({CurrentRow + 1, J}, C, M) end,
      ZippedLine = lists:zip(TLine, lists:seq(1, length(TLine))),
      NewAcc = lists:foldl(AddCellFun, Acc, ZippedLine),
      read_grid(NewAcc)
  end.

can_partition(Grid, X) ->
  EndPoints = [
    {X + K} || K <- lists:seq(1, Grid#grid.cols),
    X - K + 1 >= 1 andalso X + K =< Grid#grid.cols
    andalso (X - K + 1 == 1 orelse X + K == Grid#grid.cols)
  ],
  IsRowPalFun =
    fun _({I, {R}}) ->
      Ext = R - X,
      lists:all(
        fun _(K) -> maps:get({I, X - K + 1}, Grid#grid.map) == maps:get({I, X + K}, Grid#grid.map) end,
        lists:seq(1, Ext)
      )
    end,
  ValidEndPoints = [
    E || E <- EndPoints,
    lists:all(IsRowPalFun, [{R, E} || R <- lists:seq(1, Grid#grid.rows)])
  ],
  length(ValidEndPoints) > 0.
  

vertical(Grid) ->
  Candidates = [X || X <- lists:seq(1, Grid#grid.cols), can_partition(Grid, X)],
  case length(Candidates) of
    0 -> [0];
    _ -> Candidates
  end.

flip(C) when C == $# -> $.;
flip(C) when C == $. -> $#.

get_scores(Vertical, Horizontal) ->
  Overall = [V || V <- Vertical] ++ [100 * H || H <- Horizontal],
  [X || X <- Overall, X > 0].

process_grid(Grid) ->
  OriginalVerticals = vertical(Grid),
  OriginalHorizontals = vertical(transpose(Grid)),
  OriginalScore = hd(get_scores(OriginalVerticals, OriginalHorizontals)),
  ModifiedGrids = [
    compose_grid(maps:put({I, J}, flip(maps:get({I, J}, Grid#grid.map)), Grid#grid.map)) ||
    I <- lists:seq(1, Grid#grid.rows), J <- lists:seq(1, Grid#grid.cols)
  ],
  DifferentReflections =
    lists:flatten([get_scores(vertical(G), vertical(transpose(G))) || G <- ModifiedGrids]),
  DifferentScores =
    [X || X <- DifferentReflections, X /= OriginalScore],
  hd(DifferentScores).

solve(Acc) ->
  InputGrid = read_grid(maps:new()),
  case InputGrid of
    null -> Acc;
    Grid -> solve(Acc + process_grid(Grid))
  end.


main() ->
  R = solve(0),
  io:format("~p~n", [R]).
