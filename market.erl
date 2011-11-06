-module(market).
-compile(export_all).


start() ->
    MaxListings = 10,
    Listings = sets:to_list(sets:from_list(
        [random_symbol() || Stock <- lists:seq(1, MaxListings)]
    )),

    register(ticker, spawn(market, ticker, [])),
    register(broker, spawn(market, broker, [Listings, ticker, 1000])).


stop() ->
    broker ! stop,
    ticker ! stop.


ticker() ->
    receive
        {price_query, Broker, Symbol} ->
            Broker ! {price_answer, Symbol, random_price()},
            ticker();
        stop ->
            void;
        Other ->
            io:format("WARNING! Unexpected request: ~p~n", [Other]),
            ticker()
    end.


broker(Listings, Ticker, Interval) ->
    receive
        {price_answer, Symbol, Price} ->
            io:format("~p~n", [{Symbol, Price}]),
            broker(Listings, Ticker, Interval);
        stop ->
            void;
        Other ->
            io:format("WARNING! Unexpected request: ~p~n", [Other]),
            broker(Listings, Ticker, Interval)
    after Interval ->
            Symbol = choice(Listings),
            Ticker ! {price_query, self(), Symbol},
            broker(Listings, Ticker, Interval)
    end.


random_symbol() ->
    CharPool = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    LenPool = [1, 2, 3, 4],
    Symbol = [choice(CharPool) || S <- lists:seq(1, choice(LenPool))],
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
