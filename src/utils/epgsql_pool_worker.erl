%%%-------------------------------------------------------------------
%%% @doc PostgreSQL连接池工作进程
%%% @end
%%%-------------------------------------------------------------------
-module(epgsql_pool_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([
  start_link/1,
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {connection}).

%%--------------------------------------------------------------------
%% @doc 启动工作进程
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

%%--------------------------------------------------------------------
%% @doc 初始化工作进程
%% @end
%%--------------------------------------------------------------------
init(Args) ->
  Host = proplists:get_value(host, Args),
  Port = proplists:get_value(port, Args),
  Database = proplists:get_value(database, Args),
  Username = proplists:get_value(username, Args),
  Password = proplists:get_value(password, Args),

  % 建立数据库连接
  ConnectionOptions = [
    {host, Host},
    {port, Port},
    {database, Database},
    {username, Username},
    {password, Password}
  ],

  case epgsql:connect(ConnectionOptions) of
    {ok, Connection} ->
      {ok, #state{connection = Connection}};
    {error, Reason} ->
      {stop, Reason}
  end.

%%--------------------------------------------------------------------
%% @doc 处理同步调用
%% @end
%%--------------------------------------------------------------------
handle_call({query, Query, Params}, _From, #state{connection = Connection} = State) ->
  Result = epgsql:equery(Connection, Query, Params),
  {reply, Result, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @doc 处理异步调用
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @doc 处理其他消息
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @doc 终止进程时的清理
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{connection = Connection}) ->
  ok = epgsql:close(Connection),
  ok.

%%--------------------------------------------------------------------
%% @doc 代码热更新
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.