
-module(rest_query_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).
-include("org.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Org) ->
  supervisor:start_child(?MODULE, {list_to_atom(Org#org.orgid), {rest_query, start_link, [Org]},
      temporary, brutal_kill, worker, [rest_query, rest_query_manager]}).
%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Child = {rest_query_manager, {rest_query_manager, start_link, []},
    temporary, brutal_kill, worker, [rest_query, rest_query_manager]},
  Children = [Child],
  RestartStrategy = {one_for_one, 5, 10},
  {ok, { RestartStrategy, Children} }.

