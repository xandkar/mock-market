-module(market).
-compile(export_all).

-define(TICKER_INTERVAL, 1000).
-define(NUM_LISTINGS, 5).
-define(NUM_BROKERS, 3).
-define(MAX_SHARES_PER_TRANSACTION, 10).


%%%----------------------------------------------------------------------------
%%% Controlls
%%%----------------------------------------------------------------------------

start() ->

    % Generate listings
    Listings = sets:to_list(sets:from_list(
        [random_symbol() || _ <- lists:seq(1, ?NUM_LISTINGS)]
    )),

    % Register & spawn brokers
    lists:foreach(
        fun(BrokerName) ->
            register(BrokerName, spawn(market, broker, []))
        end,
        atoms_sequence("broker", "_", 1, ?NUM_BROKERS)
    ),

    % Register & spawn ticker
    register(ticker_proc, spawn(market, ticker, [Listings, ?TICKER_INTERVAL])).


%%
%% Sends 'stop' message to all registered procs
%%
stop() ->
    Procs = [ticker_proc] ++ atoms_sequence("broker", "_", 1, ?NUM_BROKERS),
    lists:foreach(fun(Proc) -> Proc ! stop end, Procs).


%%%----------------------------------------------------------------------------
%%% Agents
%%%----------------------------------------------------------------------------

%%
%% Announces current prices to brokers
%%
ticker(Listings, Interval) ->
    Brokers = atoms_sequence("broker", "_", 1, ?NUM_BROKERS),

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


%%
%% Listens for current prices and either buys or sells
%%
broker() ->
    Portfolio = dict:new(),
    Transactions = [],
    broker(Portfolio, Transactions).

broker(Portfolio, Transactions) ->
    {registered_name, ProcName} = erlang:process_info(self(), registered_name),
    CashBalance = lists:sum(Transactions),
    receive
        {ticker, {prices, Prices}} ->
            {Symbol, Price} = choice(Prices),
            {NewPortfolio, NewTransactions} = transaction(
                choice([buy, sell]),
                Symbol,
                Price,
                choice(lists:seq(1, ?MAX_SHARES_PER_TRANSACTION)),
                Portfolio,
                Transactions
            ),

            io:format("~p PORTFOLIO:~p~n", [ProcName, dict:to_list(NewPortfolio)]),
            io:format("~p CASH BALANCE:~p~n", [ProcName, CashBalance]),
            io:format("~n"),
            broker(NewPortfolio, NewTransactions);
        stop ->
            void;
        Other ->
            io:format("WARNING! Unexpected message: ~p~n", [Other]),
            broker(Portfolio, Transactions)
    end.


%%%----------------------------------------------------------------------------
%%% Helpers
%%%----------------------------------------------------------------------------

transaction(Type, Symbol, Price, Shares, Portfolio, PastTransactions) ->
    NewPortfolio = dict:update(
        Symbol,
        case Type of
            buy  -> fun(CurrentShares) -> CurrentShares + Shares end;
            sell -> fun(CurrentShares) -> CurrentShares - Shares end
        end,
        _InitialShares =
            case Type of
                buy  -> + Shares;
                sell -> - Shares
            end,
        Portfolio
    ),

    NewTransactions =
        case Type of
            buy  -> [-(Price * Shares) | PastTransactions];
            sell -> [+(Price * Shares) | PastTransactions]
        end,

    {NewPortfolio, NewTransactions}.


%% Generates a random stock symbol
random_symbol() ->
    CharPool = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    LenPool = [1, 2, 3, 4],
    Symbol = [choice(CharPool) || _ <- lists:seq(1, choice(LenPool))],
    Symbol.


%% Generates a random price
random_price() ->
    DigPool = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    LenPool = [1, 2, 3],
    Length = choice(LenPool),

    Dollars = [integer_to_list(choice(DigPool)) || _ <- lists:seq(1, Length)],
    Cents   = [integer_to_list(choice(DigPool)) || _ <- lists:seq(1, 2)],

    Price = string:join(
        [string:join(Dollars, ""), string:join(Cents, "")],
        "."
    ),

    list_to_float(Price).


%% Pick and return a random element from a given list
choice(List) ->
    reseed(),
    Maximum = length(List),
    Element = random:uniform(Maximum),
    lists:nth(Element, List).


%% Reseed pseudorandom number generator
reseed() ->
    random:seed(timehash(), timehash(), timehash()).


%% Generates a Unix timestamp float
timestamp() ->
    [Mega, Sec, Micro] = [integer_to_list(I) || I <- tuple_to_list(now())],
    Seconds = string:join([Mega, Sec], ""),
    Timestamp = string:join([Seconds, Micro], "."),
    list_to_float(Timestamp).


%% Generates a cryptographically unique integer based on current time
timehash() ->
    Timestamp = float_to_list(timestamp()),
    HashBin  = crypto:sha(Timestamp),
    HashList = [integer_to_list(I) || I <- binary_to_list(HashBin)],
    HashStr  = string:join(HashList, ""),
    list_to_integer(HashStr).


%% Generate a list of numerically sequential atoms:
%% [atom_1, atom_2, ...]
atoms_sequence(String, Separator, FromNum, ToNum) ->
    [
        list_to_atom(
            string:join([String, integer_to_list(I)], Separator)
        ) ||
        I <- lists:seq(FromNum, ToNum)
    ].
