-module(positioning).
-export([start/0]).

start() -> lists:reverse(solve([a, b, c], [])).

% Recursive function that applies minimax algorithm to find optimal game state.
solve([Player|RemainingPlayers], Positions) ->
  PossibleConfigurations = get_possible_configurations(Player, Positions),
  ConfigurationScores = lists:map(fun(X) -> {Player, X, score(Player, solve(RemainingPlayers, [X] ++ Positions))} end, PossibleConfigurations),
  {_, SelectedPosition, _} = lists:nth(length(ConfigurationScores), sort_scores(ConfigurationScores)),
  solve(RemainingPlayers, [SelectedPosition] ++ Positions);
solve([], Positions) -> Positions.

get_possible_configurations(Player, Positions) -> [{Player, Y} || Y <- get_available_positions(Positions)].
get_available_positions(Positions) -> lists:subtract(lists:seq(1, 100), lists:map(fun({_, X}) -> X end, Positions)).

% Functions for determining the score of the game given the current player and final game state.
score(Player, Positions) ->
  WorkingList = sort_positions(Positions),
  {Player, PlayersPosition} = lists:nth(1, lists:filter(fun({P, _Pos}) -> Player =:= P end, WorkingList)),
  PlayerIndex = index_of({Player, PlayersPosition}, WorkingList),
  get_upper_score_contribution(PlayerIndex, WorkingList, length(WorkingList), PlayersPosition) +
  get_lower_score_contribution(PlayerIndex, WorkingList, PlayersPosition).

get_upper_score_contribution(PlayerIndex, WorkingList, ListLength, PlayersPosition) ->
  if PlayerIndex =:= ListLength -> (100 - PlayersPosition);
     true -> {_, UpperScore} = lists:nth(PlayerIndex + 1, WorkingList), (UpperScore - PlayersPosition) / 2
  end.
get_lower_score_contribution(PlayerIndex, WorkingList, PlayersPosition) ->
  if PlayerIndex =:= 1 -> (PlayersPosition);
     true -> {_, LowerScore} = lists:nth(PlayerIndex - 1, WorkingList), (PlayersPosition - LowerScore) / 2
  end.

% Sorts a list of {Player, {Player, Position}, Score} 
sort_scores(L) -> lists:sort(fun(A, B) -> {_, _, AScore} = A, {_, _, BScore} = B, AScore =< BScore end, L).
% Sorts a list of {Player, Position} lowest to highest position
sort_positions(L) -> lists:sort(fun(A, B) -> {_, APos} = A, {_, BPos} = B, APos =< BPos end, L).

% Returns the index of the item in the list
index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).
