-module(part1).
-export([main/0]).
-record(hand, {rank, hand_str, reward}).

freq_to_rank() ->
  #{
    [5] => 0,
    [4, 1] => -1,
    [3, 2] => -2,
    [3, 1, 1] => -3,
    [2, 2, 1] => -4,
    [2, 1, 1, 1] => -5,
    [1, 1, 1, 1, 1] => -6
  }.

card_to_rank(C) ->
  case C of
    $T -> 10;
    $J -> 11;
    $Q -> 12;
    $K -> 13;
    $A -> 14;
    _ -> C - $0
  end.


get_card_freq_map([], Acc) -> Acc;
get_card_freq_map([Card | T], Acc) ->
  NewCardFreq = maps:get(Card, Acc, 0) + 1,
  UpdatedMap = maps:put(Card, NewCardFreq, Acc),
  get_card_freq_map(T, UpdatedMap).

get_hand_rank(CardFreqMap) ->
  maps:get(
    lists:sort(fun _(A, B) -> A > B end, maps:values(CardFreqMap)),
    freq_to_rank()
  ).

read_card_list(Acc) ->
  case io:get_line("") of
    eof -> Acc;
    Line ->
      {_, [Cards, RewardBin]} =
        re:run(Line, "(.*) (.*)\n", [{capture, all_but_first, binary}]),
      CardInts = [card_to_rank(C) || C <- binary_to_list(Cards)],
      HandRank = get_hand_rank(get_card_freq_map(CardInts, maps:new())),
      {RewardInt, _} = string:to_integer(binary_to_list(RewardBin)),
      NewHand = #hand{
        rank=HandRank,
        reward=RewardInt,
        hand_str=CardInts
      },
      read_card_list([NewHand | Acc])
  end.

compare_hands(A, B) ->
  RankA = A#hand.rank,
  RankB = B#hand.rank,
  case RankA == RankB of
    true -> A#hand.hand_str > B#hand.hand_str;
    _ -> RankA > RankB
  end.

solve() ->
  HandList = lists:sort(fun compare_hands/2, read_card_list([])),
  lists:foldl(
    fun _(A, B) -> A + B end,
    0,
    lists:map(
      fun _({Hand, Index}) -> Hand#hand.reward * Index end,
      lists:zip(HandList, lists:reverse(lists:seq(1, length(HandList))))
    )
  ).

main() ->
  R = solve(),
  io:format("~p~n", [R]).
