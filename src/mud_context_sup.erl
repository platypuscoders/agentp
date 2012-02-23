-module(mud_context_sup).
-behavior(supervisor).

-export([start_link/0]).

-export([ init/1 ]).



start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Context = {mud_context_vnode_master,
                {riak_core_vnode_master, start_link, [mud_context_vnode]},
                permanent, 5000, worker, [riak_core_vnode_master]},
    {ok, {{one_for_one, 5, 10}, [Context]}}.

