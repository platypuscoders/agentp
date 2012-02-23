-module(mud_context_vnode).
-behavior(riak_core_vnode).

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_coverage/4,
         handle_exit/3,
         handle_info/2,
         handle_handoff_data/2,
         encode_handoff_item/2]).

-record(state, {partition, key_pids=[], key_queue=dict:new(), key_dict=dict:new()}).

-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).



%% API
start_vnode(I) ->
%    io:format("Start user vnode ~p~n", [init:get_argument(config)]),
io:format("start_vnode: ~p~n", [I]),
   riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
   io:format("vnode context init: ~p~n", [Partition]),
   {ok, #state { partition=Partition}}.

-spec mud_context_vnode:handle_command
   ({'set_context', Key::string(), Module::atom(), Fun::atom(), Args::term()},
      _Sender::{pid(), term()}, State::dict()) -> {noreply, #state{}}.

handle_command({set_context, Key, Module, Fun, Args}, _Sender, State) ->
   case xdict:find(Key, State#state.key_dict) of
      false -> 
         io:format("hc: sc: ~p ~p ~p ~p~n", [Key, Module, Fun, Args]),
         {noreply, State};
      {Key, _KeyData} ->
         % Queue command to run
         NewState = queue_add(Key, Module, Fun, Args, undefined, State),
         {noreply, NewState}
   end;

handle_command({set_synched_context, Key, Module, Fun, Args}, Sender, State) ->
   case xdict:find(Key, State#state.key_dict) of
      false -> 
         io:format("hc: ssc: ~p ~p ~p ~p~n", [Key, Module, Fun, Args]),
         {reply, {error, key_not_found}, State};
      {Key, _KeyData} ->
         % Queue command to run
         NewState = queue_add(Key, Module, Fun, Args, Sender, State),
         {noreply, NewState}
   end;

handle_command({create, Key, Data}, _Sender, State) ->
   case xdict:find(Key, State#state.key_dict) of
      false -> NewState = State#state{key_dict=dict:store(Key, Data, State#state.key_dict)};
      {Key, _KeyData} -> NewState = State
   end,
   {noreply, NewState};

handle_command({delete, Key}, _Sender, State) ->
   KeyDict = dict:erase(Key, State#state.key_dict),
   {noreply, State#state{key_dict=KeyDict}};

handle_command({exists, Key}, _Sender, State) ->
   case xdict:find(Key, State#state.key_dict) of
      false -> {reply, false, State};
      {Key, _} -> {reply, true, State}
   end;


handle_command(Message, _Sender, State) ->
   ?PRINT({unhandled_command, Message}),
   {noreply, State}.

handle_handoff_command(Message, _Sender, State) ->
   ?PRINT({unhandled_handoff_command, Message}),
   {noreply, State}.

handoff_starting(_TargetNode, State) ->
   io:format("handoff_starting~n", []),
   {true, State}.

handoff_cancelled(State) ->
   io:format("handoff_cancelled~n", []),
   {ok, State}.

handoff_finished(_TargetNode, State) ->
   io:format("handoff_finished~n", []),
   {ok, State}.

handle_handoff_data(_Data, State) ->
   io:format("handoff_data~n", []),
   {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
   <<>>.

handle_coverage({set_map_context, Module, Fun, Args}, _KeySpaces, Sender, State) ->
   Results = [Module:Fun(Key, Value, Args) || {Key, Value} <- dict:to_list(State#state.key_dict)],
   riak_core_vnode:reply(Sender, lists:flatten(Results)),
   riak_core_vnode:reply(Sender, done),
   {noreply, State};


handle_coverage(Req, _KeySpaces, _Sender, State) ->
   io:format("unhandled_coverage_command: ~p~n", [Req]),
   {noreply, State}.

handle_exit(_Pid, _Reason, State) ->
   io:format("handle_exit~n", []),
   {noreply, State}.


handle_info({process_completed, {Reply, RetValue, NewKeyData}, Pid}, State) ->
   NewState = process_completed_callback(Pid, {Reply, RetValue, NewKeyData}, State),
   {ok, NewState};

handle_info(Info, State) ->
   io:format("handle_info: ~p~n", [Info]),
   {ok, State}.

is_empty(State) ->
   io:format("is_empty~n", []),
   {true, State}.

delete(State) ->
   io:format("delete~n", []),
   {ok, State}.

terminate(_Reason, _State) ->
   io:format("terminate: ~p~n ~p~n", [_Reason, _State]),
   ok.


% If the key has a pid running then add to queue, otherwise
queue_add(Key, Module, Fun, Args, Sender, State) ->
   case xlists:keyfind(Key, 1, State#state.key_pids) of
      false ->
         % Nothing is blocking on this so let's run it
         run_key_callback(Key, Module, Fun, Args, Sender, State);
      {Key, _Pid, _Sender} -> 
         % A process is already running for this key data
         % so we need to stick this call on to the back of the queue
         case xdict:find(Key, State#state.key_queue) of
            false ->
               % There is no queue yet so create and store one
               Queue = [];
            {Key, Queue} ->
               ok
         end,

         % Append the new entry for the queue to the end of the current 
         % queue
         State#state{key_queue = xdict:store(Key, State#state.key_queue, lists:append(Queue, [{Module, Fun, Args, Sender}]))}
   end.
                  
run_key_callback(Key, Module, Fun, Args, Sender, State) ->
   {Key, KeyData} = xdict:find(Key, State#state.key_dict),
   VNodePid = self(),
   Pid = spawn(fun() -> context_run(VNodePid, Module, Fun, Args, Sender, KeyData) end),
   % Store callback information 
   State#state{key_pids = xlists:keystore(Key, 1, State#state.key_pids, {Key, Pid, Sender})}.

process_completed_callback(Pid, {Reply, RetValue, NewKeyData}, State) ->
   % Get the Key for this pid
   case xlists:keyfind(Pid, 2, State#state.key_pids) of
      false ->
         error_logger:error_msg("Received a mud_context callback from a process that is not stored as the currently running process for a key.~nPid: ~p~nKeyPids: ~p~nRetValue: ~p~n", [Pid, State#state.key_pids, RetValue]),
         State;
      {Key, Pid, Sender} -> 
         % Extract the next callback in this keys queue
         case Reply of
            noreply -> 
               ok;
            reply ->
               riak_core_vnode:reply(Sender, RetValue)
         end,
         case xdict:find(Key, State#state.key_queue) of
            false ->
               % no more in the queue so clear out the key_pids
               NewKeyDict = xdict:store(Key, State#state.key_dict, NewKeyData),
               NewKeyPids = lists:keydelete(Pid, 2, State#state.key_pids), 
               State#state{key_dict=NewKeyDict, key_pids=NewKeyPids};
            {Key, [{Module, Fun, Args, Sender} | Queue]} ->
               NewState = run_key_callback(Key, Module, Fun, Args, Sender, State),
               NewState#state{key_queue = xdict:store(Key, NewState#state.key_queue, Queue)}
         end
   end.

context_run(VNodePid, Module, Fun, Args, Sender, KeyData) -> 
   case Sender of
      undefined ->
         {noreply, NewKeyData} = Module:Fun(Args, KeyData),
         VNodePid ! {process_completed, {noreply, undefined, NewKeyData}, self()};
      _ ->
         case Module:Fun(Args, Sender, KeyData) of
            {noreply, NewKeyData} ->
               VNodePid ! {process_completed, {noreply, undefined, NewKeyData}, self()};
            {reply, RetValue, NewKeyData} ->
               VNodePid ! {process_completed, {reply, RetValue, NewKeyData}, self()}
         end
   end.



