-module(part2).
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
    $J -> 1;
    $Q -> 12;
    $K -> 13;
    $A -> 14;
    _ -> C - $0
  end.

compare_hands(A, B) ->
  RankA = A#hand.rank,
  RankB = B#hand.rank,
  case RankA == RankB of
    true -> A#hand.hand_str > B#hand.hand_str;
    _ -> RankA > RankB
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

get_best_hand([], Original, Reward, CardInts, BestHand) ->
  CandidateFreqMap = get_card_freq_map(CardInts, maps:new()),
  CandidateHand =
    #hand{
      rank=get_hand_rank(CandidateFreqMap),
      reward=Reward,
      hand_str=Original
    },
  case compare_hands(CandidateHand, BestHand) of
    true -> CandidateHand;
    _ -> BestHand
  end;

get_best_hand([Card | T], Original, Reward, CardInts, BestHand) ->
  case Card of
    1 ->
        hd(
          lists:sort(
            fun compare_hands/2,
            lists:map(
              fun _(C) -> get_best_hand(T, Original, Reward, [C | CardInts], BestHand) end,
              [X || X <- lists:seq(1, 14), X /= 11]
            )
          )
        );
    _ -> get_best_hand(T, Original, Reward, [Card | CardInts], BestHand)
  end.

read_card_list(Acc) ->
  case io:get_line("") of
    eof -> Acc;
    Line ->
      {_, [CardTemplate, RewardBin]} =
        re:run(Line, "(.*) (.*)\n", [{capture, all_but_first, binary}]),
      CardTemplateInts = [card_to_rank(C) || C <- binary_to_list(CardTemplate)],
      {RewardInt, _} = string:to_integer(RewardBin),
      NewHand = get_best_hand(
        CardTemplateInts,
        CardTemplateInts,
        RewardInt,
        [],
        #hand{rank=-7, reward=0, hand_str="11111"}
      ),
      io:format("~p ~n", [NewHand]),
      read_card_list([NewHand | Acc])
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
