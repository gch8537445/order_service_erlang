%%%-------------------------------------------------------------------
%%% @doc Redis工具模块
%%% 提供Redis连接池和基本操作功能
%%% @end
%%%-------------------------------------------------------------------
-module(redis_utils).

-export([
    init_pool/0,
    get/1,
    set/2,
    setex/3,
    del/1
]).

% 连接池名称
-define(POOL_NAME, redis_pool).

%%--------------------------------------------------------------------
%% @doc 初始化Redis连接池
%% @end
%%--------------------------------------------------------------------
init_pool() ->
    try
        % 获取Redis配置
        {ok, Host} = application:get_env(order_service_erlang, redis_host),
        {ok, Port} = application:get_env(order_service_erlang, redis_port),
        {ok, Database} = application:get_env(order_service_erlang, redis_database),
        {ok, Password} = application:get_env(order_service_erlang, redis_password),
        {ok, PoolSize} = application:get_env(order_service_erlang, redis_pool_size),

        % 定义连接池选项
        PoolOptions = [
            {name, {local, ?POOL_NAME}},
            {worker_module, eredis},
            {size, PoolSize},
            {max_overflow, 10}
        ],

        % 定义工作进程选项
        WorkerOptions = [
            {host, Host},
            {port, Port},
            {database, Database},
            {password, Password},
            {reconnect_sleep, 100}  % 重连时间间隔(ms)
        ],

        % 启动连接池
        case poolboy:start_link(PoolOptions, WorkerOptions) of
            {ok, _Pid} ->
                ok;
            {error, Reason} ->
                io:format("Redis连接池启动失败: ~p~n", [Reason]),
                {error, Reason}
        end
    catch
        _:Error ->
            io:format("Redis连接池初始化错误: ~p~n", [Error]),
            {error, initialization_error}
    end.

%%--------------------------------------------------------------------
%% @doc 获取Redis键值
%% @end
%%--------------------------------------------------------------------
get(Key) ->
    try
        % 从连接池获取连接
        poolboy:transaction(?POOL_NAME, fun(Worker) ->
            % 执行GET命令
            case eredis:q(Worker, ["GET", Key]) of
                {ok, undefined} -> {ok, undefined};
                {ok, null} -> {ok, undefined};
                Result -> Result
            end
                                        end)
    catch
        _:Error ->
            io:format("Redis GET操作失败: ~p, Key: ~p~n", [Error, Key]),
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% @doc 设置Redis键值
%% @end
%%--------------------------------------------------------------------
set(Key, Value) ->
    try
        % 从连接池获取连接
        poolboy:transaction(?POOL_NAME, fun(Worker) ->
            % 执行SET命令
            eredis:q(Worker, ["SET", Key, Value])
                                        end)
    catch
        _:Error ->
            io:format("Redis SET操作失败: ~p, Key: ~p~n", [Error, Key]),
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% @doc 设置带过期时间的Redis键值
%% @param Seconds - 过期时间（秒）
%% @end
%%--------------------------------------------------------------------
setex(Key, Seconds, Value) ->
    try
        % 从连接池获取连接
        poolboy:transaction(?POOL_NAME, fun(Worker) ->
            % 执行SETEX命令
            eredis:q(Worker, ["SETEX", Key, integer_to_list(Seconds), Value])
                                        end)
    catch
        _:Error ->
            io:format("Redis SETEX操作失败: ~p, Key: ~p~n", [Error, Key]),
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% @doc 删除Redis键
%% @end
%%--------------------------------------------------------------------
del(Key) ->
    try
        % 从连接池获取连接
        poolboy:transaction(?POOL_NAME, fun(Worker) ->
            % 执行DEL命令
            eredis:q(Worker, ["DEL", Key])
                                        end)
    catch
        _:Error ->
            io:format("Redis DEL操作失败: ~p, Key: ~p~n", [Error, Key]),
            {error, Error}
    end.