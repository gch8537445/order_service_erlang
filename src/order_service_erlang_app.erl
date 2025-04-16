%%%-------------------------------------------------------------------
%%% @doc 应用主模块
%%% @end
%%%-------------------------------------------------------------------
-module(order_service_erlang_app).
-behaviour(application).

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @doc 启动应用
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    % 获取HTTP端口配置
    {ok, HttpPort} = application:get_env(order_service_erlang, http_port),
    
    % 初始化数据库连接池
    ok = db_utils:init_pool(),
    
    % 初始化Redis连接池
    ok = redis_utils:init_pool(),
    
    % 定义Cowboy路由
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/estimate_price", estimate_price_handler, []}
        ]}
    ]),
    
    % 启动Cowboy HTTP服务器
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, HttpPort}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    % 启动主监督树
    order_service_erlang_sup:start_link().

%%--------------------------------------------------------------------
%% @doc 停止应用
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    % 停止HTTP服务器
    ok = cowboy:stop_listener(http_listener),
    ok.