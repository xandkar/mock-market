%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : market_lib.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Collection of miscellaneous helper functions used throughout the
%%%           simulation.
%%%----------------------------------------------------------------------------

-module(market_lib).
-export(
    [
        transaction/3,
        random_symbol/0,
        random_price/0,
        choice/1,
        atoms_sequence/4,
        timestamp/0
    ]
).


-include("market_config.hrl").
-include("market_types.hrl").


%%-----------------------------------------------------------------------------
%% Function : transaction/3
%% Purpose  : Modifies portfolio dict and transactions list in accordance with
%%            transaction data.
%%-----------------------------------------------------------------------------
transaction(TransactionData, Portfolio, PastTransactions) ->
    #transaction{
        type=Type,
        symbol=Symbol,
        price=Price,
        shares=Shares
    } = TransactionData,

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


%%-----------------------------------------------------------------------------
%% Function : random_symbol/0
%% Purpose  : Generates a random stock symbol.
%%-----------------------------------------------------------------------------
random_symbol() ->
    CharPool = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    LenPool = [1, 2, 3, 4],
    Symbol = [choice(CharPool) || _ <- lists:seq(1, choice(LenPool))],
    Symbol.


%%-----------------------------------------------------------------------------
%% Function : random_price/0
%% Purpose  : Generates a random price.
%%-----------------------------------------------------------------------------
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


%%-----------------------------------------------------------------------------
%% Function : choice/1
%% Purpose  : Pick and return a random element from a given list.
%%-----------------------------------------------------------------------------
choice(List) ->
    reseed(),
    Maximum = length(List),
    Element = random:uniform(Maximum),
    lists:nth(Element, List).


%%-----------------------------------------------------------------------------
%% Function : reseed/0
%% Purpose  : Reseed pseudorandom number generator.
%%-----------------------------------------------------------------------------
reseed() ->
    random:seed(timehash(), timehash(), timehash()).


%%-----------------------------------------------------------------------------
%% Function : timestamp/0
%% Purpose  : Generates a Unix timestamp float.
%%-----------------------------------------------------------------------------
timestamp() ->
    [Mega, Sec, Micro] = [integer_to_list(I) || I <- tuple_to_list(now())],
    Seconds = string:join([Mega, Sec], ""),
    Timestamp = string:join([Seconds, Micro], "."),
    list_to_float(Timestamp).


%%-----------------------------------------------------------------------------
%% Function : timehash/0
%% Purpose  : Generates a cryptographically unique integer based on current
%%            time.
%%-----------------------------------------------------------------------------
timehash() ->
    Timestamp = float_to_list(timestamp()),
    HashBin  = crypto:sha(Timestamp),
    HashList = [integer_to_list(I) || I <- binary_to_list(HashBin)],
    HashStr  = string:join(HashList, ""),
    list_to_integer(HashStr).


%%-----------------------------------------------------------------------------
%% Function : atoms_sequence/4
%% Purpose  : Generate a list of numerically sequential atoms:
%%           [atom_1, atom_2, ...]
%%-----------------------------------------------------------------------------
atoms_sequence(String, Separator, FromNum, ToNum) ->
    [
        list_to_atom(
            string:join([String, integer_to_list(I)], Separator)
        ) ||
        I <- lists:seq(FromNum, ToNum)
    ].
