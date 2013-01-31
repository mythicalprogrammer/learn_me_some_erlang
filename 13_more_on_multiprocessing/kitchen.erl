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

% Abstracting the protocol
% The problem with the previous functions is that the user
% needs to understand the structure of the message that the kitchen2 process
% will accept.
%		{From, {store, Food}} ->
%		{From, {take, Food}} ->
%
% we want to abstract that:

store(Pid, Food) ->
	Pid ! {self(), {store, Food}},
	receive
		{Pid, Msg} -> Msg
	end.

take(Pid, Food) ->
	Pid ! {self(), {take, Food}},
	receive
		{Pid, Msg} -> Msg
	end.

% the above two functions take the Pid of the spawned kitchen PID and send
% that process the shell pid and the take food command, 
% basically abstracting away the protocol
% sending to the pid itself (fridge2) will take the prepared msg


% Spawning Process before we can send process msg is annoying
% we want to hide the step where we need to spawn a process

start(FoodList) ->
	spawn(?MODULE, fridge2, [FoodList]).

% so now instead of doing:
%		Pid = spawn(kitchen, fridge2, [some_food_list]).
% we can do this:
%		Pid = kitchen:start([bacon,chicken,cheese]).


%%% 
%% What if we gave a fake PID of a process that doesn't exist?
% shell freezes
%
% shell, our process, send msg to some unkonwn process
% shell switches to recieve mode and waits for a new msg
% nonexisting procses send nothing cause it's fake
% shell stuck in recieve mode forever

% to fix this, the wait forever for a msg, erlang have a time out construct
% which wait for specified amount of time before timing out

store2(Pid, Food) ->
	Pid ! {self(), {store, Food}},
	receive
		{Pid, Msg} -> Msg
	after 3000 ->
		timeout
	end.

take2(Pid, Food) ->
	Pid ! {self(), {take, Food}},
	receive
		{Pid, Msg} -> Msg
	after 3000 ->
		timeout
	end.
