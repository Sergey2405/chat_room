-define(APP, chat_room).

-define(CHAT_WINDOW_ROWS, 20).
-define(CHAT_WINDOW_COLS, 200).
-define(CHAT_ROOM_NUMBER_OF_KEPT_MESSAGES, 20).

-define(COWBOY_PORT, 8080).
-define(COWBOY_WS_IDLE_TIMEOUT, 300000).%ms

-define(TRICKSTER_BOT_INTERVAL, 600000). %ms, 10 min
-define(TRICKSTER_BOT_NAME, "Santa").
-define(TRICKSTER_RANDOM_MESSAGES, ["Happy New year",
                                    "Merry Christmas",
                                    "Congratulations!",
                                    "Have a nice day"]).
