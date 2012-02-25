-module(mud_context).
-export([ create/2, delete/1, exists/1, set_context/4, set_synched_context/4, set_map_context/3 ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-spec mud_context:create(Key::term(), Data::term()) -> 'ok'.
create(Key, Data) ->
   send_command(Key, {create, Key, Data}).

-spec mud_context:delete(Key::term()) -> 'ok'.
delete(Key) ->
   send_command(Key, {delete, Key}).

-spec mud_context:exists(Key::term()) -> 'true' | 'false'.
exists(Key) ->
   send_synched_command(Key, {exists, Key}).

-spec mud_context:set_context(Key::term(), Module::atom(), 
                              Fun::atom(), Arg::term()) -> 'ok'.
set_context(Key, Module, Fun, Arg) ->
   send_command(Key, {set_context, Key, Module, Fun, Arg}).

-spec mud_context:set_synched_context(Key::term(), Module::atom(), Fun::atom(), Arg::term()) -> term().
set_synched_context(Key, Module, Fun, Arg) ->
   send_synched_command(Key, {set_synched_context, Key, Module, Fun, Arg}).

-spec mud_context:set_map_context(Module::atom(), Fun::atom(), Arg::term()) -> term().
set_map_context(Module, Fun, Arg) ->
   send_map({set_map_context, Module, Fun, Arg}).


-spec mud_context:send_command(Key::term(), Command::term()) -> 'ok'.
send_command(Key, Command) ->
   CKey = chash:key_of(term_to_binary(Key)),
   NVal = 1,
   [Pref] = riak_core_apl:get_apl(CKey, NVal, mud_context),
   riak_core_vnode_master:command(Pref, Command, mud_context_vnode_master),
   ok.


-spec mud_context:send_synched_command(Key::term(), Command::term()) -> term().
send_synched_command(Key, Command) ->
   CKey = chash:key_of(term_to_binary(Key)),
   NVal = 1,
   [Pref] = riak_core_apl:get_apl(CKey, NVal, mud_context),
   riak_core_vnode_master:sync_command(Pref, Command, mud_context_vnode_master, infinity).

-spec mud_context:send_map(Comand::{atom(), atom(), atom(), term()}) -> term().
send_map(Command) ->
   mud_context_map_fsm_sup:start_map_fsm(node(), [{raw, mk_reqid(), self()}, [Command, 10000, plain]]).

mk_reqid() -> erlang:phash2(erlang:now()).
   

-ifdef(TEST).
basic_test_() ->
   {setup, 
      fun setup/0,
      fun test_cleanup/1,
      [
         fun basic_test_case/0
      ]
   }.

basic_test_case() -> 
   Key = {mud_player, "Jeff"},
   ?assert(create(Key, [{a,1}]) == ok).

setup() ->
   io:format("SETTING UP TEST~n", []),
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

test_cleanup(_A) -> 
   application:stop(mud_context),
   io:format("Cleaned up~n", []),
   ok.
-endif.