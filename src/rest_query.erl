%% Copyright
-module(rest_query).
-author("pfeairheller").
-behavior(gen_server).

%% API
-export([register/3, soql_query/2]).

%%Management
-export([start_link/1, stop/0]).

%%gen server callbacks
-export([init/1, terminate/2, handle_call/3]).
-include("org.hrl").

start_link(Org) ->
  Name = atom_to_list(?MODULE) ++ "_" ++ Org#org.orgid,
  gen_server:start_link({local, list_to_atom(Name)}, ?MODULE, Org, []).

stop() ->
  gen_server:cast(?MODULE, stop).

register(OrgId, Env, RefreshToken) ->
  rest_query_manager:insert(OrgId, Env, RefreshToken).

soql_query(OrgId, Soql) ->
  case rest_query_manager:lookup(OrgId) of
    {ok, Pid} ->
      gen_server:call(Pid, {soql_query, Soql});
    {error, instance} -> {error, invalid_org}
  end.

%%gen server callbacks
init(StartArgs) ->
  {ok, StartArgs}.

terminate(_Reason, _LoopData) ->
  ok.

handle_call({soql_query, Soql}, _From, #org{access_token = AccessToken, instance_url = InstanceUrl} = LoopData) ->
  ParamString = string:join(["q", http_uri:encode(Soql)], "="),
  Url = string:join([string:concat(InstanceUrl, "/services/data/v23.0/query/"), ParamString], "?"),
  Headers = [{"Authorization", string:join(["OAuth", AccessToken], " ")}],

  {ParamList} = case httpc:request(get, {Url, Headers}, [{connect_timeout, 120000}, {timeout, 120000}], []) of
    {ok, {_Status, _Headers, Body}} -> ejson:decode(Body);
    {error, _Reason} -> {[]}
  end,

  Reply = ParamList,
  {reply, Reply, LoopData}.
