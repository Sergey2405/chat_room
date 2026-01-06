-module(chat_room_handler).
-behaviour(cowboy_websocket).

-export([init/2, init/3, handle/2, terminate/3]).
-export([websocket_init/3,
         websocket_handle/2, websocket_handle/3,
         websocket_info/2, websocket_info/3,
         websocket_terminate/3]).

-include("chat_room.hrl").

init( Req, State ) ->
    {_, Body, _} = cowboy_req:read_body(Req),
    case cowboy_req:header(<<"upgrade">>,Req) of
        <<"websocket">> ->
            % CryptedUserName = parse_qs(<<"crypteduser">>, Req),
            UserName = parse_qs(<<"username">>, Req),
            % case CryptedUserName of
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
    home_page(enter, Body, Req, State);
init(Method, <<"/chat_room">>, Body, Req, State) when ((Method =:= <<"POST">>) or
                                                       (Method =:= <<"GET">>)) ->
    case get_body(<<"textusername">>, Body) of
        undefined ->
            %% js request absent.
            %% a user has already entered the room before.
            % CryptedUserName = parse_qs(<<"crypteduser">>, Req),
            UserName = parse_qs(<<"username">>, Req),  % TODO: username
            % case chat_room_server:get_connected_user(CryptedUserName) of
            case chat_room_server:get_connected_user(UserName) of
                false ->
                    %% Re-enter User name.
                    home_page(does_not_exist, Body, Req, State);
                % {UserName, CryptedUserName} ->
                UserName ->
                    chat_room(Method, UserName, Body, Req, State)
            end;
        Value ->
            %% js request when a user is entering the room.
            % CryptedUserName = parse_qs(<<"crypteduser">>, Req),
            UserName = parse_qs(<<"username">>, Req),
            % case chat_room_server:get_connected_user(CryptedUserName) of
            case chat_room_server:get_connected_user(UserName) of   %% TODO: boolean()
                % {Value, CryptedUserName} ->
                %     %% already exists.
                %     %% but we get here from chat_room_page since it might not been reloaded!
                %     %% GET method is forcible set.
                %     chat_room(<<"GET">>, Value, Body, Req, State);
                %   _ ->
                %     %% normal case.
                %     %% we get here from home page.
                %     %% lets check again.
                %     % case chat_room_server:connect_user({Value, CryptedUserName}) of
                %     case chat_room_server:connect_user({Value, UserName}) of
                %         already_exists -> home_page(already_exists, Body, Req, State);
                %         ok -> chat_room(Method, Value, Body, Req, State)
                %     end
                false ->
                    %% normal case.
                    %% we get here from home page.
                    %% lets chack again.
                    % case chat_room_server:connect_user({Value, CryptedUserName}) of
                    % case chat_room_server:connect_user({Value, UserName}) of
                    case chat_room_server:connect_user(UserName) of
                        already_exists -> home_page(already_exists, Body, Req, State);
                        ok -> chat_room(Method, Value, Body, Req, State)
                    end;
                UserName ->
                    %% already exists.
                    %% but we get here from chat_room_page since it might not been reloaded!
                    %% GET method is forcible set.
                    chat_room(<<"GET">>, Value, Body, Req, State)
            end
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
    if Reason == timeout -> chat_room_server:delete_pid();
       true -> ok
    end.

home_page(Flag, _Body, Req, State) ->
    {HTTPResponse, WelcomeText} =
        case Flag of
            enter -> {200, "Welcome. Please enter your name."};
            already_exists -> {401, "Please enter under another user since he has already connected."};
            does_not_exist -> {401, "Please enter user name again since you have tried to send a message under not connected user."}
        end,
    % Host = binary:bin_to_list(cowboy_req:host(Req)),

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
            % chat_room_server:send_message({TextUserName, Message}),
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
