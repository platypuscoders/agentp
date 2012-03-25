-module(mud_stm).
-export([ setup/1, setup/2, exists/1, remove/1, add/2 ]).
-export([ get/2, take/2, put/2, get_all/1, set_all/2 ]).
-export([ ctx_setup/2, ctx_put/2, ctx_take/3, ctx_get/3, ctx_add/2, ctx_get_all/3, ctx_set_all/2, ctx_exists/3 ]).

-export([ dump/1, ctx_dump/3 ]).
-export([ map/3, ctx_map_identity/3, ctx_map_type/3, ctx_map_type_sub_value/3, ctx_map_sub_value/3 ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%
% Create an STM that runs in a vnode.
% The STM key is defined by the tuple {mud_stm, UniqueId}
% If Data is passed in it should be of the form: [{Key, Value}] 
% Type = value | index
%
setup(STMKey) -> 
   setup(STMKey, []).

setup(STMKey, Data = []) -> 
   mud_context:create({mud_stm, STMKey}, []),
   io:format("mud_stm:setup(~p , ~p)~n", [STMKey, Data]),
   mud_context:set_context({mud_stm, STMKey}, mud_stm, ctx_setup, Data),
   STMKey.

exists(STMKey) ->
   mud_context:exists({mud_stm, STMKey}).

map(Module, Fun, Args) ->
   mud_context:set_map_context(Module, Fun, Args),
   receive
      {_, {results, Results}} -> Results
   end.

dump(STMKey) ->
   mud_context:set_synched_context({mud_stm, STMKey}, mud_stm, ctx_dump, []).

get_all(STMKey) ->
   mud_context:set_synched_context({mud_stm, STMKey}, mud_stm, ctx_get_all, []).

set_all(STMKey, Data) ->
   mud_context:set_context({mud_stm, STMKey}, mud_stm, ctx_set_all, Data).

% Remove an STM and it's data from it's vnode
remove(STMKey) ->
   mud_context:delete({mud_stm, STMKey}).

% Add data into an STM. Any keys that already exist in the STM will be ignored
add(STMKey, Data) ->
io:format("add: IN: ~p~n", [Data]),
   Ret = mud_context:set_context({mud_stm, STMKey}, mud_stm, ctx_add, Data),
io:format("add: OUT: ~p~n", [Data]),
   Ret.

% Get data for a passed in keys, will block if key is 'taken' from the STM
get(STMKey, Key) -> 
io:format("get IN: ~p ~p~n", [STMKey, Key]),
   Ret = mud_context:set_synched_context({mud_stm, STMKey}, mud_stm, ctx_get, Key),
io:format("get OUT: ~p ~p~n", [STMKey, Key]),
   Ret.

% Get the data from the STM for a keys and mark that key as 'taken'
% This will block any other gets or takes until the data is 'put' back.
take(STMKey, Key) -> 
io:format("take IN: ~p ~p~n", [STMKey, Key]),
   Ret = mud_context:set_synched_context({mud_stm, STMKey}, mud_stm, ctx_take, Key),
io:format("take OUT: ~p ~p~n", [STMKey, Key]),
   Ret.

% Put data back into the STM. The key must have been previously 'taken'
put(STMKey, {Key, Value}) ->
io:format("put IN: ~p ~p~n", [STMKey, Key]),
   Ret = mud_context:set_context({mud_stm, STMKey}, mud_stm, ctx_put, {Key, Value}),
io:format("put OUT: ~p ~p~n", [STMKey, Key]),
   Ret.



% Funs for handling calls in the context of the STM's vnode
% See calling functions for information on behavior
%
% External data structure is [{Key, Value, Type}]
% Internal STM data structure is [{Key, Value, Type, TakeList}]
% Where TakeList is the list of pids waiting on a take (The top
% of the list is the current pid that has taken it)
% After a put, the top is popped off the list and the next 
% pid in the take list is replied to with the value
%

ctx_setup(Data, _) -> 
   {noreply, coerce_data_in(Data)}.

ctx_map_identity({mud_stm, Key}, Value, _Args) -> 
   [{Key, Value}];
ctx_map_identity(_, _, _) ->
   [].

ctx_map_type({mud_stm, {Type, Key}}, Value, Type) ->
   [{{Type, Key}, Value}];
ctx_map_type(_, _, _) ->
   [].

ctx_map_sub_value({mud_stm, Key}, Value, {SubKey, SubValue}) ->
   case lists:keyfind(SubKey, 1, Value) of
      false -> [];
      {SubKey, SubValue, _} -> [{Key, Value}];
      _ -> []
   end;
ctx_map_sub_value(_, _, _) ->
   [].
   
ctx_map_type_sub_value({mud_stm, Key}, Value, {Type, SubKey, SubValue}) ->
   case ctx_map_type({mud_stm, Key}, Value, Type) of
      [] -> [];
      [{Key, V}] -> 
         case lists:keyfind(SubKey, 1, V) of
            false -> [];
            {SubKey, SubValue, _} -> [{Key, Value}];
            _ -> []
         end;
      _ -> []
   end;
ctx_map_type_sub_value(_, _, _) ->
   [].
               
   

ctx_dump([], _Sender, KeyData) ->
   {reply, KeyData, KeyData}.

ctx_get_all([], _Sender, KeyData) ->
   {reply, coerce_data_out(KeyData), KeyData}.

ctx_set_all(NewData, KeyData) ->
   NewDataIn = lists:keysort(1, coerce_data_in(NewData)),
   CurrentData = lists:keysort(1, KeyData),

   % Recurse over new data and store it 
   RepFun = fun({K, V, T}, Acc) ->
         lists:keystore(K, 1, Acc, {K, V, T})
      end,

   NewDataOut = lists:foldl(RepFun, CurrentData, NewDataIn),
   {noreply, NewDataOut}.
   

%ctx_get_all_map([], _Sender, KeyData) ->
%   {reply, KeyData, KeyData}.

ctx_add(NewData, KeyData) ->
   NewDataIn = lists:keysort(1, coerce_data_in(NewData)),
   CurrentData = lists:keysort(1, KeyData),

   % Recurse over new data and store it if it doesn't already exist
   RepFun = fun({K, V, T}, Acc) ->
         case lists:keymember(K, 1, Acc) of
            true -> Acc;
            false ->
               lists:keystore(K, 1, Acc, {K, V, T})
         end
      end,

   NewDataOut = lists:foldl(RepFun, CurrentData, NewDataIn),

   {noreply, NewDataOut}.

ctx_exists(Key, _Sender, KeyData) ->
   case lists:keyfind(Key, 1, KeyData) of
      false -> 
         {reply, false, KeyData};
      _ -> 
         {reply, true, KeyData}
   end.

ctx_get(Key, _Sender, KeyData)  -> 
   case lists:keyfind(Key, 1, KeyData) of 
      false -> 
         {reply, {error, not_found}, KeyData};
      {Key, Value, TakeList} ->
         {reply, coerce_data_out({Key, Value, TakeList}), KeyData}
   end.


ctx_take(Key, Sender, KeyData) ->
   case lists:keyfind(Key, 1, KeyData) of 
      false -> 
         {reply, {error, not_found}, KeyData};
      {Key, Value, TakeList} ->
         case TakeList of
            [] -> 
               {reply, 
               coerce_data_out({Key, Value, TakeList}), 
               add_to_takelist({Key, Value, TakeList}, {Sender, take}, KeyData)};
            _ ->
            io:format("ctx_take: ~p TL: ~p~n", [Key, TakeList]),
               {noreply, add_to_takelist({Key, Value, TakeList}, {Sender, take}, KeyData)}
         end
   end.


ctx_put({Key, Value}, KeyData) -> 
   case lists:keyfind(Key, 1, KeyData) of 
      false -> 
         {noreply, KeyData};
      {Key, _, TakeList} ->
         case TakeList of
            [] -> 
               io:format("ERROR: mud_stm:put key: ~p that is not taken~n", [Key]),
               {noreply, KeyData};
            [{Sender, take} | NewTakeList] ->
               riak_core_vnode:reply(Sender, {coerce_data_out({Key, Value, NewTakeList})}),
               {noreply, update_takelist({Key, Value, NewTakeList}, KeyData)};
            _ ->
               io:format("ERROR: mud_stm:put key: ~p that is has malformed taken list: ~p~n", [Key, TakeList]),
               {noreply, KeyData}
         end
   end.

coerce_data_in(Data) ->   
   [{Key, Value, []} || {Key, Value} <- Data].

coerce_data_out(Data) when is_list(Data)->
   [{Key, Value} || {Key, Value, _} <- Data];
coerce_data_out({Key, Value, _}) ->
   {Key, Value}.

add_to_takelist({Key, Value, TakeList}, Update, KeyData) ->
   Ret = lists:keystore(Key, 1, KeyData, {Key, Value, TakeList ++ [Update]}),
%   io:format("NewTakeList: (~p) ~p~n", [Update, Ret]),
   Ret.

% Run through everything in the take list and respond through the first take
update_takelist({Key, Value, []}, KeyData) ->
   lists:keystore(Key, 1, KeyData, {Key, Value, []});
update_takelist({Key, Value, [{Sender, get} | RestOfTakeList]}, KeyData) ->
   riak_core_vnode:reply(Sender, {coerce_data_out({Key, Value, RestOfTakeList})}),
   update_takelist({Key, Value, RestOfTakeList}, KeyData);
update_takelist({Key, Value, [{Sender, take} | RestOfTakeList]}, KeyData) ->
   riak_core_vnode:reply(Sender, {coerce_data_out({Key, Value, RestOfTakeList})}),
   lists:keystore(Key, 1, KeyData, {Key, Value, RestOfTakeList}).

-ifdef(TEST).
basic_test_() ->
   {setup, 
      fun setup/0,
      fun test_cleanup/1,
      [
         fun basic_test_case_0/0,
         fun basic_map_test_0/0,
         fun basic_par_test_0/0
      ]
   }.


% Run through a basic test of creating, getting, taking, putting, etc
basic_test_case_0() ->
   [
      ?assertEqual(a, mud_stm:setup(a)),
      ?assertEqual(true, mud_stm:exists(a)),
      ?assertEqual(false, mud_stm:exists(b)),
      ?assertEqual(ok, mud_stm:add(a, [{val, 1}])),
      ?assertEqual({val, 1}, mud_stm:get(a, val)),
      ?assertEqual({val, 1}, mud_stm:take(a, val)),
      ?assertEqual(ok, mud_stm:put(a, {val, 2})),
      ?assertEqual({val, 2}, mud_stm:get(a, val)),
      ?assertEqual(ok, mud_stm:remove(a)),
      ?assertEqual(false, mud_stm:exists(a))
   ].

basic_map_test_0() ->
   [
      ?assertEqual(true, true)
   ].

basic_par_test_0() ->
   KeyList = [a, a],

   % Create keys
   mud_stm:setup(a),
   mud_stm:add(a, [{val, 1}]),

   % Base fun
   Pid = self(),
   Fun = fun(A) -> 
      {val, Val} = mud_stm:take(A, val),
      mud_stm:put(A, {val, Val + 1}),
      Pid ! A
   end,


   % Spawn threads
   [spawn(fun() -> Fun(Key) end) || Key <- KeyList],

   % Receive response
   Ret = [begin
      receive
         Key -> ok
      end
   end || Key <- KeyList],

   RetVal = [
      ?assertEqual([ok || _ <- KeyList], Ret)
   ],

   mud_stm:remove(a),
   RetVal.

setup() ->
   application:start(crypto),
   application:start(public_key),
   application:start(ssl),
   application:start(sasl),
   application:start(os_mon),
   application:start(webmachine),
   application:start(riak_sysmon),
   application:start(compiler),
   application:start(syntax_tools),
   application:start(lager),
   application:start(riak_core),
   application:start(mud_context).


test_cleanup(_A) -> ok.
-endif.
