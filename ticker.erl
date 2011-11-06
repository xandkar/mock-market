-module(ticker).
-compile(export_all).


start() ->
    Price = random_price(),
    Symbol = random_symbol(),

    Output = [[{symbol, Symbol}, {price, Price}]],

    io:format("~p~n", Output).


random_symbol() ->
    CharPool = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    Symbol = [choice(CharPool) || S <- lists:seq(1, 3)],
    Symbol.


random_price() ->
    DigPool = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    LenPool = [1, 2, 3],

    Dollars = [
        integer_to_list(choice(DigPool)) ||
        S <- lists:seq(1, choice(LenPool))
    ],

    Cents = [
        integer_to_list(choice(DigPool)) ||
        S <- lists:seq(1, 2)
    ],

    Price = string:join(
        [string:join(Dollars, ""), string:join(Cents, "")],
        "."
    ),

    list_to_float(Price).


%%-----------------------------------------------------------------------------
%% Pick and return a random element from a given list
%%-----------------------------------------------------------------------------
choice(List) ->
    Maximum = length(List),
    Element = random:uniform(Maximum),
    lists:nth(Element, List).
