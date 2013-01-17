-module(functions).
-compile(export_all). %% replace with -export() Later, for God's sake!
-export([head/1, second/1]).

head([H|_]) -> H.

second([_, X|_]) -> X.
