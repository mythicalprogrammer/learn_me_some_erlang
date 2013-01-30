-module(kitchen).
-compile(export_all).

% The following exercise is to show how to hold state in a process

% What we're trying to do:
% A process that act like a fridge.
%
% process will allow two operations:
% 	- storing food in the fridge
%	- taking food from the fridge

fridge1() ->
	receive
		{From, {store, _Food}} ->
			From ! {self(), ok},
			fridge1();
		{From, {take, _Food}} ->
			%% uh....
			From ! {self(), not_found},
			fridge1();
		terminate ->
			ok
	end.

% The above function doesn't have state to store food
% so we'll fix that by adding state

fridge2(FoodList) ->
	receive
		{From, {store, Food}} ->
			From ! {self(), ok},
			fridge2([Food|FoodList]);
		{From, {take, Food}} ->
			case lists:member(Food, FoodList) of
				true ->
					From ! {self(), {ok, Food}},
					fridge2(lists:delete(Food, FoodList));
				false ->
					From ! {self(), not_found},
					fridge2(FoodList)
			end;
		terminate ->
			ok
	end.
