%%%-------------------------------------------------------------------
%%% @doc 数据库工具模块
%%% 提供PostgreSQL数据库连接池和查询功能
%%% @end
%%%-------------------------------------------------------------------
-module(db_utils).

-export([
    init_pool/0,
    query/2
]).

% 连接池名称
-define(POOL_NAME, pg_pool).

%%--------------------------------------------------------------------
%% @doc 初始化PostgreSQL连接池
%% @end
%%--------------------------------------------------------------------
init_pool() ->
    % 获取数据库配置
    {ok, Host} = application:get_env(order_service_erlang, pg_host),
    {ok, Port} = application:get_env(order_service_erlang, pg_port),
    {ok, Database} = application:get_env(order_service_erlang, pg_database),
    {ok, Username} = application:get_env(order_service_erlang, pg_username),
    {ok, Password} = application:get_env(order_service_erlang, pg_password),
    {ok, PoolSize} = application:get_env(order_service_erlang, pg_pool_size),

    % 定义连接池选项
    PoolOptions = [
        {name, {local, ?POOL_NAME}},
        {worker_module, epgsql_pool_worker},
        {size, PoolSize},
        {max_overflow, 10}
    ],

    % 定义工作进程选项
    WorkerOptions = [
        {host, Host},
        {port, Port},
        {database, Database},
        {username, Username},
        {password, Password}
    ],

    % 启动连接池
    case poolboy:start_link(PoolOptions, WorkerOptions) of
        {ok, _Pid} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc 执行SQL查询
%% @param Query - SQL查询语句
%% @param Params - 查询参数列表
%% @return {ok, Columns, Rows} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
query(Query, Params) ->
    % 从连接池获取连接
    poolboy:transaction(?POOL_NAME, fun(Worker) ->
        % 执行查询
        case gen_server:call(Worker, {query, Query, Params}) of
            {ok, _Count, Columns, Rows} ->
                % 对于INSERT/UPDATE/DELETE语句
                {ok, Columns, Rows};
            {ok, Columns, Rows} ->
                % 对于SELECT语句
                {ok, Columns, Rows};
            Error ->
                Error
        end
                                    end).