-module(chat_room_server).
-behaviour(gen_server).

%% API
-export([start_link/0,
         stop/0]).
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).
-export([connect_user/1,
         save_pid/1,
         delete_pid/0, delete_pid/1,
         is_user_connected/1,
         get_messages/0,
         send_message/1,
         send_message_to_everyone/1]).
-export([start_trickster_bot/3]).

-include("chat_room.hrl").

-record(state, {messages = [] :: [{string(), string()}],
                connected_users = [] :: [string()],
                websockets = [] :: [{pid(), string()}] }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

init([]) ->
    case application:get_env(?APP, trickster_bot, false) of
        true ->
            spawn_link(?MODULE, start_trickster_bot,
                       [application:get_env(?APP, trickster_bot_interval, ?TRICKSTER_BOT_INTERVAL),
                        application:get_env(?APP, trickster_bot_name, ?TRICKSTER_BOT_NAME),
                        application:get_env(?APP, trickster_random_messages, ?TRICKSTER_RANDOM_MESSAGES)]);
        false -> no_trickstrer_bot
    end,
    {ok, #state{}}.

handle_call({connect_user, UserName}, _From, State) ->
    Users = State#state.connected_users,
    case lists:member(UserName, Users) of
        false -> {reply, ok, State#state{connected_users = [UserName|State#state.connected_users]}};
        true ->  {reply, already_exists, State}
    end;
handle_call({is_user_connected, UserName}, _From, State) ->
    {reply, lists:member(UserName, State#state.connected_users), State};
handle_call({save_pid,{Pid, UserName}}, _From, State) ->
    %% Pid is key,
    %% Username is value.
    %% One user might have several connections.
    case lists:member(UserName, State#state.connected_users) of
        false -> {reply, unknown_user, State};
        true  -> {reply, ok, State#state{websockets = [{Pid, UserName}|State#state.websockets]}}
    end;
handle_call(get_messages, _From, State) ->
    {reply, State#state.messages, State};
%% not used.
handle_call({send_message, NewMessage = {_User, _Message}}, _From, State) ->
    {reply, ok,
     State#state{messages = lists:sublist([NewMessage|State#state.messages], 1,
                                          application:get_env(?APP, number_of_kept_messages,
                                                              ?CHAT_ROOM_NUMBER_OF_KEPT_MESSAGES))}};
handle_call({delete_pid, Pid}, _From, State) ->
    Websockets = State#state.websockets,
    NewWebSockets = lists:keydelete(Pid, 1, Websockets),

    Users = State#state.connected_users,

    NewUsers =
        case lists:keyfind(Pid, 1, Websockets) of
            false ->
                %% corresponding user is not found.
                %% do nothing.
                Users;
            {_, TokenUserName} ->
                case lists:keyfind(TokenUserName, 2, NewWebSockets) of
                    false ->
                        %% no more connection for the user exists.
                        %% delete him.
                        gen_server:cast(?MODULE, {send_message_to_everyone, {TokenUserName, exited}}),
                        [UserName || UserName <- Users, UserName =/= TokenUserName];
                    {_TokenPid, TokenUserName} ->
                        %% one more connection for the user.
                        %% do nothing
                        Users
              end
        end,

    {reply, ok, State#state{websockets = NewWebSockets,
                            connected_users = NewUsers}};
handle_call({send_message_to_everyone, {PidOrUserName, Message}}, _From, State) ->
    %% can be sent asynchrounously either.
    {reply, ok, do_send_message_to_everyone({PidOrUserName, Message}, State)};
handle_call(Request, _From, State) ->
    logger:error("Unexpected Request ~p", [Request]),
    {reply, ok, State}.


handle_cast({send_message_to_everyone, {PidOrUserName, Message}}, State) ->
    %% can be sent synchrounously either.
    {noreply, do_send_message_to_everyone({PidOrUserName, Message}, State)};
handle_cast(Msg, State) ->
    logger:info("Unexpected Msg ~p", [Msg]),
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(Reason, _State) ->
    logger:info("Terminate with reason", [Reason]),
    ok.

do_send_message_to_everyone({PidOrUserName, Message}, State) ->
    Websockets = State#state.websockets,
    TextUserName =
      if is_list(PidOrUserName) ->
            PidOrUserName;
         is_pid(PidOrUserName) ->
            case lists:keyfind(PidOrUserName, 1, Websockets) of
              false -> "Anonimous";
              {_, Name} -> Name
            end;
         true -> "Anonimous"
       end,
    TextMessage =
        case Message of
            entered -> TextUserName ++ " entered.";
            exited -> TextUserName ++ " exited.";
            _Value -> TextUserName ++ ": " ++ Message
        end,
    NewWebsockets =
          lists:foldl(fun({Pid, _} = Websocket, Acc) ->
                            case is_process_alive(Pid) of
                                true ->
                                    Pid ! TextMessage,
                                    [Websocket|Acc];
                                false ->
                                    gen_server:cast(?MODULE, {delete_pid, Pid}),
                                    Acc
                            end;
                          (_, Acc) -> Acc
                      end,
                      [], Websockets),
    State#state{messages = lists:sublist([{TextUserName, Message}|State#state.messages], 1,
                                         application:get_env(?APP, number_of_kept_messages,
                                                             ?CHAT_ROOM_NUMBER_OF_KEPT_MESSAGES)),
                websockets = NewWebsockets}.

connect_user(UserName) ->
    gen_server:call(?MODULE, {connect_user, UserName}).

save_pid(UserName) when is_binary(UserName) ->
    save_pid(binary:bin_to_list(UserName));
save_pid(UserName) when is_list(UserName) ->
    save_pid({self(), UserName});
save_pid({Pid, UserName}) when is_pid(Pid) ->
    gen_server:call(?MODULE, {save_pid,{Pid, UserName}}).

delete_pid() ->
    delete_pid(self()).

delete_pid(Pid) ->
    gen_server:call(?MODULE, {delete_pid, Pid}).

is_user_connected(UserName) ->
    gen_server:call(?MODULE, {is_user_connected, UserName}).

get_messages() ->
    Messages = gen_server:call(?MODULE, get_messages),
    TextArea = 
        fun ({User, entered}, Acc) ->
                User ++ " entered.\n" ++ Acc;
            ({User, exited}, Acc) ->
                User ++ " exited.\n" ++ Acc;
            ({User, Message}, Acc) ->
                User ++ ": " ++ Message ++ "\n" ++ Acc;
            (_, Acc) -> Acc
        end,
    lists:foldl(TextArea, "", Messages).

send_message(Message) ->
    gen_server:call(?MODULE, {send_message, Message}).

send_message_to_everyone(Message) when is_binary(Message) ->
    send_message_to_everyone({self(), binary:bin_to_list(Message)});
send_message_to_everyone({text, Message}) ->
    send_message_to_everyone(Message);
% send_message_to_everyone([{text, Message}|_]) ->
%     send_message_to_everyone(Message);
send_message_to_everyone(Message) when is_list(Message) or is_atom(Message) ->
    send_message_to_everyone({self(), Message});
send_message_to_everyone({PidOrUserName, Message}) when (is_list(PidOrUserName) or
                                                         is_pid(PidOrUserName)) and
                                                         is_binary(Message) ->
    send_message_to_everyone({PidOrUserName, binary:bin_to_list(Message)});
send_message_to_everyone({PidOrUserName, Message}) when (is_list(PidOrUserName) or
                                                         is_pid(PidOrUserName)) and
                                                         (is_list(Message) or
                                                          is_atom(Message)) ->
    gen_server:call(?MODULE, {send_message_to_everyone, {PidOrUserName, Message}}).

start_trickster_bot(Interval, UserName, RandomMessages) ->
    loop_trickster_bot(Interval, UserName, RandomMessages).

loop_trickster_bot(Interval, UserName, RandomMessages) ->
    RandomMessage = fetch_random_message(RandomMessages),
    send_message_to_everyone({UserName, RandomMessage}),
    timer:send_after(rand:uniform(Interval), self(), send_another_message),
    receive send_another_message->
        loop_trickster_bot(Interval, UserName, RandomMessages)
    end.

fetch_random_message(Messages) ->
    [Message] = lists:sublist(Messages, rand:uniform(length(Messages)), 1),
    Message.
