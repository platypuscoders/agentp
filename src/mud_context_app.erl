-module(mud_context_app).
-export([start/2, stop/1]).
-behavior(application).

start(_StartType, _StartArgs) ->
	case mud_context_map_fsm_sup:start_link() of
		{error, Reason} ->
			{error, Reason};
	_ ->
	    case mud_context_sup:start_link() of
	        {ok, Pid} ->
   	        	riak_core:register(mud_context, [{vnode_module, mud_context_vnode}]),
   	        	riak_core_node_watcher:service_up(mud_context, self()),
            	{ok, Pid};
        	{error, Reason} ->
            	{error, Reason}
        end
    end.

stop(_State) ->
    ok.

