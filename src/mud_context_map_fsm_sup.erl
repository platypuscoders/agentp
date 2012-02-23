-module(mud_context_map_fsm_sup).

-behaviour(supervisor).

-export([start_map_fsm/2]).
-export([start_link/0]).
-export([init/1]).

start_map_fsm(Node, Args) ->
    supervisor:start_child({?MODULE, Node}, Args).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    MapFsmSpec = {undefined,
               {riak_core_coverage_fsm, start_link, [mud_context_map_fsm]},
               temporary, 5000, worker, [mud_context_map_fsm]},

    {ok, {{simple_one_for_one, 10, 10}, [MapFsmSpec]}}.