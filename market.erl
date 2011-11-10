-module(market).
-compile(export_all).

-define(MAX_LISTINGS, 5).
-define(MAX_BROKERS, 3).
-define(TICKER_INTERVAL, 1000).


%%%----------------------------------------------------------------------------
%%% Controlls
%%%----------------------------------------------------------------------------

start() ->
    Listings = sets:to_list(sets:from_list(
        for(1, ?MAX_LISTINGS, fun() -> random_symbol() end)
    )),

    lists:foreach(
        fun(BrokerName) ->
            register(BrokerName, spawn(market, broker, []))
        end,
        atoms_sequence("broker", "_", 1, ?MAX_BROKERS)
    ),

    register(ticker_proc, spawn(market, ticker, [Listings, ?TICKER_INTERVAL])).


stop() ->
    Brokers = atoms_sequence("broker", "_", 1, ?MAX_BROKERS),

    lists:foreach(fun(Broker) -> Broker ! stop end, Brokers),
    ticker_proc ! stop.


%%%----------------------------------------------------------------------------
%%% Agents
%%%----------------------------------------------------------------------------

ticker(Listings, Interval) ->
    Brokers = atoms_sequence("broker", "_", 1, ?MAX_BROKERS),

    receive
        stop ->
            void;
        Other ->
            io:format("WARNING! Unexpected request: ~p~n", [Other]),
            ticker(Listings, Interval)
    after Interval ->
            Prices = [{Symbol, random_price()} || Symbol <- Listings],
            Message = {ticker, {prices, Prices}},

            % Broadcast prices to brokers
            lists:foreach(
                fun(Broker) ->
                        Broker ! Message
                end, 
                Brokers
            ),

            ticker(Listings, Interval)
    end.


broker() ->
    Portfolio = dict:new(),
    Transactions = [],
    broker(Portfolio, Transactions).

broker(Portfolio, Transactions) ->
    Buy = fun(
            GivenSymbol,
            GivenPrice,
            GivenShares,
            GivenPortfolio,
            GivenTransactions) ->

                NewPortfolio = dict:update(
                    GivenSymbol,
                    fun(Shares) -> GivenShares + Shares end,
                    _InitialShares = 0,
                    GivenPortfolio
                ),

                NewTransactions =
                    [-(GivenPrice * GivenShares) | GivenTransactions],

                {NewPortfolio, NewTransactions}
    end,

    receive
        {ticker, {prices, Prices}} ->
            lists:foreach(
                fun({Symbol, Price}) ->
                        {NewPortfolio, NewTransactions} = Buy(
                            Symbol, Price, 1, Portfolio, Transactions
                        ),
                        io:format("~p~n", [NewPortfolio]),
                        io:format("~p~n", [NewTransactions])
                end,
                Prices
            ),

            broker();
        stop ->
            void;
        Other ->
            io:format("WARNING! Unexpected message: ~p~n", [Other]),
            broker()
    end.


%%%----------------------------------------------------------------------------
%%% Helpers
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


%% Generate a list of numerically sequential atoms:
%% [atom_1, atom_2, ...]
atoms_sequence(String, Separator, FromNum, ToNum) ->
    [
        list_to_atom(
            string:join([String, integer_to_list(I)], Separator)
        ) ||
        I <- lists:seq(FromNum, ToNum)
    ].
