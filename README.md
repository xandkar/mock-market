# Mock Market #


DESCRIPTION
===========

An attempt at a very simple stock market simulation.

Generates a set of random listings, then ticker sets a random price for each
listing every interval and announces it to the brokers. Each broker randomly
chooses a listing and randomly chooses to either buy or sell a random amount of
shares of it (accumulating a portfolio and a list of monetary transactions).
Scribe records the details of every transaction to the log file.

See *src/market_config.hrl* for configurable values.


USAGE
=====

Compile:

    make


Start:

```sh
$ erl -pa ebin
> application:start(mock_market).
```


Stop:

```sh
> application:stop(mock_market).
```
