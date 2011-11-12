-define(TICKER_INTERVAL, 1000).
-define(NUM_LISTINGS, 5).
-define(NUM_BROKERS, 3).
-define(MAX_SHARES_PER_TRANSACTION, 10).
-define(LOG_FIELD_DELIMITER, "\t").
-define(PATH_DIR__DATA, "data").
-define(PATH_FILE__LOG,
    string:join([?PATH_DIR__DATA, "transactions.log"], "/")
).
