-module(calc).
-export([rpn/1]).

rpn(L) when is_list(L) ->
    [Res] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
    Res.

rpn("+", [N1,N2|S]) -> [N2+N1|S];
rpn("-", [N1,N2|S]) -> [N2-N1|S];
rpn("*", [N1,N2|S]) -> [N2*N1|S];
rpn("/", [N1,N2|S]) -> [N2/N1|S];
rpn("^", [N1,N2|S]) -> [math:pow(N2,N1)|S];
rpn("ln", [N|S])    -> [math:log(N)|S];
rpn("log10", [N|S]) -> [math:log10(N)|S];
rpn("sum", L) -> [lists:sum(L)];
rpn("prod", Stack)  -> [lists:foldl(fun erlang:'*'/2, 1, Stack)];
%rpn("prod", Stack)  -> [lists:foldl('*', 1, Stack)];
%rpn("prod", Stack)  -> [lists:foldl(fun erlang:'+'/2, 1, Stack)];
%rpn("prod", Stack)  -> [lists:foldl(fun erlang:'-'/2, 1, Stack)];
%rpn("prod", Stack)  -> [lists:foldl(fun erlang:'/'/2, 1, Stack)];
%rpn("prod", Stack)  -> [lists:foldl(fun erlang:'*'/2, [], Stack)];
%rpn("prod", Stack)  -> [lists:foldl(fun erlang:'*'/2, 0, Stack)];
%rpn("prod", Stack)  -> [lists:foldl(fun erlang:'*'/2, 2, Stack)];
%rpn("prod", Stack)  -> [erlang:print(lists:foldl(fun erlang:'*'/2, H, [H|T]))];
%rpn("prod", Stack)  -> [erlang:display(lists:foldl(fun erlang:'*'/2, 1, Stack))];
%rpn("prod", Stack)  -> erlang:display([lists:foldl(fun erlang:'*'/2, 1, Stack)]);
%rpn("prod", Stack)  -> [lists:foldl(erlang:display(fun erlang:'*'/2), 1, Stack)];
%rpn("prod", Stack)  -> [lists:foldl(fun erlang:'*'/2), 1, erlang:display(Stack)];
rpn(X, Stack) -> [read(X)|Stack].

%% read(String()) -> Int() | Float()
read(N) ->
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_} -> F
    end.
