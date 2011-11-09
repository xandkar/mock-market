-module(market).
-compile(export_all).


%%%----------------------------------------------------------------------------
%%% Controlls
%%%----------------------------------------------------------------------------

start() ->
    MaxListings = 5,
    MaxBrokers = 3,
    Listings = sets:to_list(sets:from_list(
        for(1, MaxListings, fun() -> random_symbol() end)
    )),

    lists:foreach(
        fun(I) ->
            register(
                list_to_atom(string:join(["broker", integer_to_list(I)], "_")),
                spawn(market, broker, [])
            )
        end,
        lists:seq(1, MaxBrokers)
    ),

    Interval = 1000,
    register(ticker_proc, spawn(market, ticker, [Listings, Interval])).


stop() ->
    MaxBrokers = 3,
    Brokers = [
        list_to_atom(string:join(["broker", integer_to_list(I)], "_")) ||
        I <- lists:seq(1, MaxBrokers)
    ],

    lists:foreach(fun(Broker) -> Broker ! stop end, Brokers),
    ticker_proc ! stop.


%%%----------------------------------------------------------------------------
%%% Agents
%%%----------------------------------------------------------------------------
ticker(Listings, Interval) ->
    MaxBrokers = 3,
    Brokers = [
        list_to_atom(string:join(["broker", integer_to_list(I)], "_")) ||
        I <- lists:seq(1, MaxBrokers)
    ],

    receive
        stop ->
            void;
        Other ->
            io:format("WARNING! Unexpected request: ~p~n", [Other]),
            ticker(Listings, Interval)
    after Interval ->
            Prices = [{Symbol, random_price()} || Symbol <- Listings],
            Message = {ticker, {prices, Prices}},
            lists:foreach(
                fun(Broker) ->
                        Broker ! Message
                end, 
                Brokers
            ),
            ticker(Listings, Interval)
    end.


broker() ->
    receive
        {ticker, {prices, Prices}} ->
            io:format("~p~n", [{self(), choice(Prices)}]),
            broker();
        stop ->
            void;
        Other ->
            io:format("WARNING! Unexpected message: ~p~n", [Other]),
            broker()
    end.


%%%----------------------------------------------------------------------------
%%% Helper functions
%%%----------------------------------------------------------------------------

%% Generates a random stock symbol
random_symbol() ->
    CharPool = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    LenPool = [1, 2, 3, 4],
    Symbol = for(1, choice(LenPool), fun() -> choice(CharPool) end),
    Symbol.


%% Generates a random price
random_price() ->
    DigPool = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    LenPool = [1, 2, 3],

    Dollars = for(1, choice(LenPool),
        fun() -> integer_to_list(choice(DigPool)) end
    ),

    Cents = for(1, 2,
        fun() -> integer_to_list(choice(DigPool)) end
    ),

    Price = string:join(
        [string:join(Dollars, ""), string:join(Cents, "")],
        "."
    ),

    list_to_float(Price).


%% Pick and return a random element from a given list
choice(List) ->
    Maximum = length(List),
    Element = random:uniform(Maximum),
    lists:nth(Element, List).


%% Repeat Function(), Max times, accumulating a list of its return values
for(Max, Max, Function) ->
    [Function()];

for(Init, Max, Function) ->
    [Function() | for(Init + 1, Max, Function)].
