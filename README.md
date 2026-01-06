chat_room
=====

An OTP application

Build
-----

    $ rebar3 compile
Run
-------------
```
    $ rebar3 shell
```
Description
-------------

The application is written on erlang, based on web server used cowboy library.
It is a chat with two main pages:
- <server_ip>:<port_num>
- <server_ip>:<port_num>/chat_room?username=...
Where server_ip - server IP,
port_num - number of port, 8080 by default.

On the second page user user will be  connected to the chat
where he will be allowed to send and receive all messages from all the connected users immediately.
The connection is done through WebSocket.

“Trickster bot” is present in the system optionally. It send periodically a random message to the chat.

Parameters
-------------

chat_room parameters - general parameters for the application.

number_of_kept_messages, positive integer, 20 by default  - number of last messages that system keeps.

trickster_bot – boolean parameter, false by default. Defines whether the bot is enabled.

trickster_bot_interval - interval ms, 300000 by default. Defines random interval 0 up to its value when the bot should send a random messages

trickster_bot_name – trickster bot nickname, string.

trickster_random_messages, list of strings. Set of messages which the bot can send.


cowboy parameters

port – port number, 8080 by default.

ws_idle_timeout idle timeout for WebSocket, ms, 300000 by default.


What should be done in latest releases:

- gen_server is user for message handling.
Kept info (connected users, latest send messages) does not survive restart.
Another mechanism for restore the info should be used, for example Redis and so on.

- User should register before connecting.

- Implement several chat rooms.

- Improve web interface!!!
