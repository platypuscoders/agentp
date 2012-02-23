-module(mud_context_map_fsm).

-behaviour(riak_core_coverage_fsm).

-export([init/2,
         process_results/2,
         finish/2]).

-type from() :: {atom(), req_id(), pid()}.
-type req_id() :: non_neg_integer().

-record(state, {results=[] :: [term()],
                client_type :: plain | mapred,
                from :: from()}).

%% @doc Return a tuple containing the ModFun to call per vnode,
%% the number of primary preflist vnodes the operation
%% should cover, the service to use to check for available nodes,
%% and the registered name to use to access the vnode master process.
init(From={_, _, ClientPid}, [Req, Timeout, ClientType]) ->
    case ClientType of
        %% Link to the mapred job so we die if the job dies
        mapred ->
            link(ClientPid);
        _ ->
            ok
    end,
    %% Construct the bucket listing request

    {Req, allup, 1, 1, mud_context, mud_context_vnode_master, Timeout,
     #state{client_type=ClientType, from=From}}.


process_results(done, StateData) ->
    {done, StateData};
process_results(Results,
                StateData=#state{results=ResultsAcc}) ->
%    io:format("results (~p): ~p", [ResultsAcc, Results]),
    {ok, StateData#state{results=lists:append(ResultsAcc, Results)}};
%    {ok, StateData#state{results=sets:union(sets:from_list(Results),
%                                               ResultsAcc)}};

process_results({error, Reason}, _State) ->
    {error, Reason}.

finish({error, Error},
       StateData=#state{client_type=ClientType,
                        from={raw, ReqId, ClientPid}}) ->
%    io:format("map finish: (~p) error: ~p~n", [Error, StateData]),
    case ClientType of
        mapred ->
            %% An error occurred or the timeout interval elapsed
            %% so all we can do now is die so that the rest of the
            %% MapReduce processes will also die and be cleaned up.
            exit(Error);
        plain ->
            %% Notify the requesting client that an error
            %% occurred or the timeout has elapsed.
            ClientPid ! {ReqId, Error}
    end,
    {stop, normal, StateData};
finish(clean,
       StateData=#state{results=Results,
                        client_type=ClientType,
                        from={raw, ReqId, ClientPid}}) ->
    case ClientType of
        mapred ->
            luke_flow:add_inputs(ClientPid, Results),
            luke_flow:finish_inputs(ClientPid);
        plain ->
            ClientPid ! {ReqId, {results, Results}}
    end,
    {stop, normal, StateData}.
