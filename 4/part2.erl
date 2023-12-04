-module(part2).
-export([solve/0]).

process_line(Line) ->
  [_, CardInfo] = string:split(string:replace(Line, "  ", " ", all), ":"),
  [Winning, Rest] = 
    lists:map(
      fun _(X) -> string:split(string:trim(X), " ", all) end,
      string:split(CardInfo, "|")
    ),
  sets:size(
    sets:intersection(
      sets:from_list(Winning),
      sets:from_list(Rest)
    )
  ).

update_for_list([], _, CardCopies) -> CardCopies;
update_for_list([Card | T], CurrentCard, CardCopies) ->
  update_for_list(
    T,
    CurrentCard,
    maps:put(
      Card,
      maps:get(Card, CardCopies, 0) + maps:get(CurrentCard, CardCopies),
      CardCopies
    )
  ).

update_card_copies(CurrentCard, ComputedScore, CardCopies) ->
  update_for_list(
    lists:seq(CurrentCard + 1, CurrentCard + ComputedScore),
    CurrentCard,
    maps:put(
      CurrentCard,
      maps:get(CurrentCard, CardCopies, 0) + 1,
      CardCopies
    )
  ).

main(CurrentCard, CardCopies) ->
  case io:get_line("") of
    eof ->
      lists:foldl(
        fun _(A, B) -> A + B end,
        0,
        maps:values(CardCopies)
      );
    Line ->
      CurrentScore = process_line(Line),
      UpdatedCardCopies =
        update_card_copies(
          CurrentCard,
          CurrentScore,
          CardCopies
        ),
      main(CurrentCard + 1, UpdatedCardCopies)
  end.

solve() ->
  R = main(0, maps:new()),
  io:format("~p~n", [R]).
