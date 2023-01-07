-module(chat_room_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-define(SPEC_WORKER(Id), #{id => Id, start => {Id, start_link, []}}).
-define(SPEC_WORKER(Id, Args), #{id => Id, start => {Id, start_link, Args}}).
-define(SPEC_WORKER(Id, M, F, Args), #{id => Id, start => {M, F, Args}}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpecs = [?SPEC_WORKER(chat_room_server, [])],
    {ok, { {one_for_one, 5, 10}, ChildSpecs} }.
