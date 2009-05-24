-module(rapperl_collect_server).
-include("rapperl_constants.hrl").
-behaviour(gen_server).

-export([start/0
        ,start_link/0
        ,begin_run/0
        ,collect/2
        ,items/0
        ,result/1]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,code_change/3
        ,terminate/2]).

% State:
%   Runner -> RunData
% RunData:
%   {Item, Value} -> Times
% TODO - add indexes, it's slower than it should/has to be

start() ->
   gen_server:start({local, ?COLLECT_SRV}, ?MODULE, [], []).

start_link() ->
   gen_server:start_link({local, ?COLLECT_SRV}, ?MODULE, [], []).

begin_run() ->
   gen_server:call(?COLLECT_SRV, {begin_run, self()}).

collect(Item, Value) ->
   gen_server:cast(?COLLECT_SRV, {collect, Item, Value, self()}).

items() ->
   gen_server:call(?COLLECT_SRV, {items, self()}).

result(Item) ->
   gen_server:call(?COLLECT_SRV, {result, Item, self()}).

init(_) ->
   {ok, dict:new()}.

% Initialize new test run
% or reset old test run data
handle_call({begin_run, Runner}, _, State) ->
   {reply, ok, dict:store(Runner, dict:new(), State)};

% Get items collected during test run
handle_call({items, Runner}, _, State) ->
   RunData  = dict:fetch(Runner, State),
   ItemVals = dict:fetch_keys(RunData),
   Items    = [Item || {Item, Val} <- ItemVals],
   Unique   = ordsets:from_list(Items),
   {reply, Unique, State};

handle_call({result, Item, Runner}, _, State) ->
   RunData  = dict:fetch(Runner, State),
   ItemVals = dict:fetch_keys(RunData),
   Values   = [begin
                  Times = dict:fetch(ItemVal, RunData),
                  {Item, Val} = ItemVal,
                  {Val, Times}
               end || ItemVal <- ItemVals],
   {reply, Values, State}.
      
handle_cast({collect, Item, Value, Runner}, State) ->
   RunData    = dict:fetch(Runner, State),
   NewRunData = dict:update_counter({Item, Value}, 1, RunData),
   NewState   = dict:store(Runner, NewRunData, State),
   {noreply, NewState}.

handle_info(Msg, State) ->
   {noreply, State}.

code_change(_, State, _) ->
   {ok, State}.

terminate(Reason, State) ->
   'I DIED?!?'.

