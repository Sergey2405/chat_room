-module(chat_room_app).
-behaviour(application).
-export([start/2, stop/1]).

-include("chat_room.hrl").

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
      {'_', [
        {"/chat_room", chat_room_handler, []},
        {"/", chat_room_handler, []}
       ]}
    ]),

    cowboy:start_clear(https, [{port, application:get_env(cowboy, port, ?COWBOY_PORT)}],
        #{env => #{dispatch => Dispatch}}),
    chat_room_sup:start_link().

stop(_State) ->
    ok.
