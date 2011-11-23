% A naive extension of minimax to solve a multi-player positioning game.
% Author: Cody Rioux
% Email: cody.rioux@gmail.com
% Site: http://codyrioux.github.com
%
% A word of caution, this algorithm has a high computational complexity and takes a very long
% time to run with inputs greater than four players. I mean it. I'd recommend making supper
% after setting the algorithm loose on six or more players. Ten or more I'd recommend letting
% it run while you're on vacation.
%
% The slowness of this algorithm can be blamed on three things:
% 1 - High computational complexity
% 2 - Naive Implementation
% 3 - My general lack of understanding on how recursion and concurrency in erlang works
%
% The nice part about point three is that it offers a learning opportunity for me.
% As I work through learning erlang I expect to improve the recursion and create a concurrent
% version of this that can be run on multiple processors or even multiple machines for larger
% problem inputs.

-module(positioning).
-export([start/0]).

% Two lists are passed to solve.
% The first is a list of players, any token can be used to represent players.
% The second is a list of positions and must be in the form of [{a,25},{b,75},{c,74}]
start() -> lists:reverse(solve([a, b, c], [])).


%%%%%%%%%%%%%%%%%%%%%
%%% The Algorithm %%%
%%%%%%%%%%%%%%%%%%%%%

solve([Player|RemainingPlayers], Positions) ->
  PossibleConfigurations = get_possible_configurations(Player, Positions),
  ConfigurationScores = lists:map(fun(X) -> {Player, X, score(Player, solve(RemainingPlayers, [X] ++ Positions))} end, PossibleConfigurations),
  {_, SelectedPosition, _} = lists:nth(length(ConfigurationScores), sort_scores(ConfigurationScores)),
  solve(RemainingPlayers, [SelectedPosition] ++ Positions);
solve([], Positions) -> Positions.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scoring Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%

get_possible_configurations(Player, Positions) -> [{Player, Y} || Y <- get_available_positions(Positions)].
get_available_positions(Positions) -> lists:subtract(lists:seq(1, 100), lists:map(fun({_, X}) -> X end, Positions)).

% Sorts a list of {Player, {Player, Position}, Score} 
sort_scores(L) -> lists:sort(fun(A, B) -> {_, _, AScore} = A, {_, _, BScore} = B, AScore =< BScore end, L).

% Sorts a list of {Player, Position} lowest to highest position
sort_positions(L) -> lists:sort(fun(A, B) -> {_, APos} = A, {_, BPos} = B, APos =< BPos end, L).

% Returns the index of the item in the list
index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).
