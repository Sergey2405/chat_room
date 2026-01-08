-module(chat_room_websocket_handler).
-behaviour(cowboy_websocket).

-export([init/2, init/3, handle/2, terminate/3]).
-export([websocket_init/3,
         websocket_handle/2, websocket_handle/3,
         websocket_info/2, websocket_info/3,
         websocket_terminate/3]).

-include("chat_room.hrl").

init(Req, State) ->
    {_, Body, _} = cowboy_req:read_body(Req),
    case cowboy_req:header(<<"upgrade">>,Req) of
        <<"websocket">> ->
            UserName = parse_qs(<<"username">>, Req),
            case UserName of
                undefined ->
                    logger:debug("Is trying to save pid to unknown User. Reload page"),
                    init(cowboy_req:method(Req), cowboy_req:path(Req), Body, Req, State);
                Value ->
                    chat_room_server:save_pid({maps:get(pid, Req, self()), Value}),
                    {cowboy_websocket, Req, State,
                     #{idle_timeout => application:get_env(cowboy, ws_idle_timeout, ?COWBOY_WS_IDLE_TIMEOUT)}}
        end;
      _ ->
        init(cowboy_req:method(Req), cowboy_req:path(Req), Body, Req, State)
    end.


init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

init(<<"GET">>, <<"/">>, Body, Req, State) ->
    home_page(welcome, Req, State);
init(Method, <<"/chat_room">>, Body, Req, State) when ((Method =:= <<"POST">>) or
                                                       (Method =:= <<"GET">>)) ->
    UserName = parse_qs(<<"username">>, Req),
    case get_body(<<"textusername">>, Body) of
        undefined ->
            %% js request absent.
            %% a user has already entered the room before.
            case chat_room_server:is_user_connected(UserName) of
                true  -> chat_room(Method, UserName, Body, Req, State);
                false -> home_page(please_register, Req, State)
            end;
        Value ->
            %% js request when a user is entering the room.
            chat_room_server:connect_user(UserName),
            chat_room(Method, Value, Body, Req, State)
    end;
init(_Method, _Path, _Body, Req, State) ->
    NewReq = cowboy_req:reply(405, #{}, <<"method not allowed">>, Req),
    {ok, NewReq, State}.

handle(Req, State) ->
    logger:debug("Request not expected: ~p", [Req]),
    {ok, Req2} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),
    {ok, Req2, State}.

websocket_init(_TransportName, Req, _Opts) ->
    logger:debug("websocket_init/3 with Req: ~p" , [Req]),
    {ok, Req, undefined_state}.

websocket_handle(Req, State) ->
    logger:debug("websocket_handle with Req: ~p",[Req]),
    chat_room_server:send_message_to_everyone(Req),
    {ok, State}.

websocket_handle({text, Msg}, Req, State) ->
    logger:debug("Got Data: ~p", [Msg]),
    {reply, {text, << "responding to ", Msg/binary >>}, Req, State, hibernate };

websocket_handle(_Any, Req, State) ->
    {reply, {text, << "what?">>}, Req, State, hibernate }.

websocket_info(Req, State) ->
    {reply, {text, Req}, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
    logger:debug("websocket info/3"),
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

terminate(Reason, _Req, _State) ->
    logger:debug("terminate with Reason: ~p",[Reason]),
    case Reason of
        timeout -> chat_room_server:delete_pid();
        {remote, _, _} -> chat_room_server:delete_pid();
        _ -> ok
    end.

home_page(welcome, Req, State) ->
    home_page({200, "Welcome. Please enter your name."}, Req, State);
home_page(please_register, Req, State) ->
    home_page({401, "Please enter user name again since you have tried to send a message under not connected user."},
              Req, State);
home_page({HTTPResponse, WelcomeText}, Req, State) ->
    RawHtml =  case file:read_file("src/home.html") of
                {ok, BinaryConenent} ->
                    BinaryConenent;
                {error, Reason} ->
                    Reason
               end,
    Html = re:replace(RawHtml, "WELCOME TEXT", WelcomeText),
    NewReq = cowboy_req:reply(HTTPResponse,#{}, Html, Req),
    {ok, NewReq, State}.

% chatroom post message.
chat_room(Method, TextUserName, Body, Req, State) ->
    Message =
        case get_body(<<"usermsg">>, Body) of
            undefined -> entered;
            Value -> Value
        end,
    case Method of
        <<"POST">> ->
            chat_room_server:send_message_to_everyone({TextUserName, Message});
        <<"GET">> -> ok
    end,
    TextArea = chat_room_server:get_messages(),
    Host = binary:bin_to_list(cowboy_req:host(Req)),

    RawHtml =  case file:read_file("src/chat_room.html") of
            {ok, BinaryConenent} ->
                BinaryConenent;
            {error, Reason} ->
                Reason
           end,
    Html1 = re:replace(RawHtml, "CHAT WINDOW ROWS", integer_to_list(?CHAT_WINDOW_ROWS)),
    Html2 = re:replace(Html1, "CHAT WINDOW COLS", integer_to_list(?CHAT_WINDOW_COLS)),
    Html3 = re:replace(Html2, "TEXT AREA", TextArea),
    Html  = re:replace(Html3, "HOST PORT", Host ++ ":" ++ integer_to_list(application:get_env(cowboy, port, ?COWBOY_PORT))),

    NewReq = cowboy_req:reply(200, #{}, Html, Req),
    {ok, NewReq, State}.

get_body(Key, UriBody) ->
    try binary:bin_to_list(proplists:get_value(Key, uri_string:dissect_query(UriBody))) of
        Value -> Value
    catch
        _:_ -> undefined
    end.

parse_qs(Key, Req) ->
    try binary:bin_to_list(proplists:get_value(Key, cowboy_req:parse_qs(Req))) of
        Value -> Value
    catch
        _:_ -> undefined
    end.
