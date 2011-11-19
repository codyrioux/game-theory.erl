-module(positioning).
-export([start/0]).

start() ->
%  io:format("Solving the positioning problem for X entrants.~nAssuming percentages are continuous whole numbers between 0 and 100.~n"),
  get_score_maximizing_positions([26, 75, 100, 50]).

solve(PlayerCount) -> solve(1, PlayerCount).
solve(X, PlayerCount) when X =< PlayerCount -> 0 .
solve(X, PlayerCount, Selections) when X =:= PlayerCount ->
  lists:nth(1, get_score_maximizing_positions(Selections)).

% Gives us all the available positions that will maximize our score given the current selections
get_score_maximizing_positions(Selections) ->
  AvailablePositions = lists:subtract(lists:seq(1, 100), Selections),
  MaxScore = lists:max(lists:map(fun(Y) -> score(Y, Selections) end, AvailablePositions)),
  lists:filter(fun(X) -> score(X, Selections) =:= MaxScore end, AvailablePositions).


% Determines your score based on the given position and the current selections.
score(Position, Selections) ->
  get_upper_score_contribution(Position, Selections) + get_lower_score_contribution(Position, Selections).

get_upper_score_contribution(Position, Selections) ->
  WorkingList = lists:sort([Position] ++ Selections),
  PositionIndex = index_of(Position, WorkingList),
  Length = length(WorkingList),
  if
      PositionIndex =:= Length ->
        (100 - Position); %We are the top and we get all the score from us to 100 from above
      true ->
        (lists:nth(PositionIndex + 1, WorkingList) - Position) / 2
  end.

get_lower_score_contribution(Position, Selections) -> 
  WorkingList = lists:sort([Position] ++ Selections),
  PositionIndex = index_of(Position, WorkingList),
  if
      PositionIndex =:= 1 ->
        (Position); % We are the bottom and get all the score between zero and us
      true ->
        (Position - lists:nth(PositionIndex - 1, WorkingList)) / 2
  end.

% Helper Functions
index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).
