-module(dolphins).
-compile(export_all).

% Assuming we recieve do_a_flip atom, fish atom, or anything else, send
% this io msg.
%
% once you spawn a process which runs the function, this function
% will recieve one msg, after completing it, it kills itself
dolphin1() ->
	receive
		do_a_flip ->
			io:format("How about no?~n");
		fish ->
			io:format("So long and thanks for all the fish!~n");
		_ ->
			io:format("Heh, we're smarter than you humans.~n")
	end.

% This version demonstrate instead of outputting to the console, 
% why not recieve a msg from the process it self? 
%
% Note this function assume that it would recieve a tuple in the from of
% {From (PID), atom of either do_a_flip or fish} or else
% output to console a msg
dolphin2() ->
	receive
		{From, do_a_flip} ->
			From ! "How about no?";
		{From, fish} ->
			From ! "So long and thanks for all the fish!";
		_ ->
			io:format("Heh, we're smarter than you humans.~n")
	end.

% This version address the fact that we can only only send one message and the
% process will send only one reply back before dying. 
% we don't want to start a new process for each call
% "want to reply for every message and keep going afterwards"
dolphin3() ->
	receive
		{From, do_a_flip} ->
			From ! "How about no?",
			dolphin3();
		{From, fish} ->
			From ! "So long and thanks for all the fish!";
		_ ->
			io:format("Heh, we're smarter than you humans.~n"),
			dolphin3()
	end.
