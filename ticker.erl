-module(ticker).
-compile(export_all).


start() ->
    CharPool = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    DigPool = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    LenPool = [1, 2, 3],

    Symbol = [choice(CharPool) || S <- lists:seq(1, 3)],
    Price = [choice(DigPool) || S <- lists:seq(1, choice(LenPool))],

    Output = [[{symbol, Symbol}, {price, Price}]],

    io:format("~p~n", Output).


%%-----------------------------------------------------------------------------
%% Pick and return a random element from a given list
%%-----------------------------------------------------------------------------
choice(List) ->
    Maximum = length(List),
    Element = random:uniform(Maximum),
    lists:nth(Element, List).
