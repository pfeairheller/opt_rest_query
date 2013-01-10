%% Copyright
-module(rest_query_manager).
-author("pfeairheller").

-behavior(gen_server).

%% API
-export([insert/3, lookup/1]).

%%Management
-export([start_link/0, stop/0]).

%%gen server callbacks
-export([init/1, terminate/2, handle_call/3]).

-include("org.hrl").

%% These belong in the .app file, but I can't check in the values so they are OS env variables for now.
%% for .app.src: {env, [{client_id, "ASDFSAFADS"}, {client_secret, "092431431243"}]}
%% -define(CLIENTID, application:get_env(client_id)).
%% -define(CLIENT_SECRET, application:get_env(client_secret)).

-define(CLIENTID, os:getenv("CLIENT_ID")).
-define(CLIENT_SECRET, os:getenv("CLIENT_SECRET")).

init(_StartArgs) ->
  ets:new(orgTable, [named_table, {keypos, 1}]),
  {ok, [{production, "https://login.salesforce.com"}, {sandbox, "https://test.salesforce.com"}]}.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

insert(OrgId, Env, RefreshToken) ->
  gen_server:call(?MODULE, {insert, OrgId, Env, RefreshToken}).

lookup(OrgId) ->
  case ets:lookup(orgTable, OrgId) of
    [{OrgId, Pid}] -> {ok, Pid};
    [] -> {error, instance}
  end.

terminate(_Reason, _LoopData) ->
  ok.

handle_call({insert, OrgId, Env, RefreshToken}, _From, LoopData) when (Env == production) or (Env == sandbox) ->
  {_, Site} = lists:keyfind(Env, 1, LoopData),

  Identity = oauth2:token_from_refresh(?CLIENTID, ?CLIENT_SECRET, Site, RefreshToken),
  {_, AccessToken} = lists:keyfind(access_token, 1, Identity),
  {_, InstanceUrl} = lists:keyfind(instance_url, 1, Identity),

  Org = #org{
      orgid = OrgId,
      access_token = AccessToken,
      instance_url = InstanceUrl,
      refresh_token = RefreshToken,
      sfdc_env = Env},
  {ok, Pid} = rest_query_sup:start_child(Org),
  ets:insert(orgTable, {OrgId, Pid}),

  {reply, ok, LoopData}.
