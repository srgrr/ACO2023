-module(part2_eff).
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

get_cycle([Mov], M, Current, Graph, Steps, Acc) when Mov == $\n ->
  get_cycle(M, M, Current, Graph, Steps, Acc);

get_cycle([Mov | T] = List, M, Current, Graph, Steps, Acc) ->
  Key = {length(List), Current},
  case maps:is_key(Key, Acc) of
    true ->
      CycleInit = maps:get(Key, Acc),
      NormMap = maps:from_list(
        [{{Kl, Kc}, V - CycleInit} || {{Kl, Kc}, V} <- maps:to_list(Acc), V >= CycleInit]
      ),
      maps:put("init_steps", CycleInit, NormMap);
    _ ->
      [L, R] = maps:get(Current, Graph),
      Next = next_step(Mov, L, R),
      NextMap = maps:put(Key, Steps, Acc),
      get_cycle(T, M, Next, Graph, Steps + 1, NextMap)
  end.

get_cycle_info(C) ->
  #{
    "Z" =>
      lists:sort(
        [Value || {{_, Key}, Value} <- maps:to_list(C), lists:last(Key) == $Z]
      ),
    "cycle_length" => lists:foldl(fun erlang:max/2, 0, maps:values(C)) + 1,
    "init_steps" => maps:get("init_steps", C)
  }.

% k * length + offset = target
% k = (target - offset) / length
can_match(CycleInfo, TargetSteps) ->
  Length = maps:get("cycle_length", CycleInfo),
  Offset = hd(maps:get("Z", CycleInfo)) + maps:get("init_steps", CycleInfo),
  ((TargetSteps - Offset) rem Length) == 0.

% solo funciona si cada ciclo de forma individual pasa por una unica casilla
% que termina en Z, si hay mas de una hay que probar todas las combinaciones
get_first_coincidence(Iteration, [HeadInfo | CycleInfos] = AllInfos) ->
  FirstCycleSteps =
    hd(maps:get("Z", HeadInfo)) + maps:get("init_steps", HeadInfo) +
    Iteration * maps:get("cycle_length", HeadInfo),
  RestCanMatch =
    lists:all(fun _(C) -> can_match(C, FirstCycleSteps) end, CycleInfos),
  case RestCanMatch of
    true -> FirstCycleSteps;
    _ -> get_first_coincidence(Iteration + 1, AllInfos)
  end.

solve() ->
  {Movements, _} = {io:get_line(""), io:get_line("")},
  Graph = read_graph(maps:new()),
  Origins = [X || X <- maps:keys(Graph), lists:last(X) == $A],
  Cycles = [get_cycle(Movements, Movements, X, Graph, 0, maps:new()) || X <- Origins],
  CycleInfos = [get_cycle_info(C) || C <- Cycles],
  io:format("~p~n", [CycleInfos]),
  FirstCycle = get_first_coincidence(0, CycleInfos),
  FirstCycle.

main() ->
  R = solve(),
  io:format("~p ~n", [R]).
