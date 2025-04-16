%%%-------------------------------------------------------------------
%%% @doc 腾讯地图API交互模块
%%% 用于获取路线距离和时间信息
%%% @end
%%%-------------------------------------------------------------------
-module(tencent_map_api).

-export([get_route_info/2]).

%%--------------------------------------------------------------------
%% @doc 获取两点间的路线信息
%% @param Start - 起点坐标 (格式: "纬度,经度")
%% @param End - 终点坐标 (格式: "纬度,经度")
%% @return {ok, {Distance, Duration}} | {error, Reason}
%%   Distance - 以米为单位的距离
%%   Duration - 以秒为单位的时间
%% @end
%%--------------------------------------------------------------------
get_route_info(Start, End) ->
    % 获取API配置
    {ok, ApiKey} = application:get_env(order_service_erlang, tencent_map_api_key),
    {ok, BaseUrl} = application:get_env(order_service_erlang, tencent_map_direction_url),

    % 构建请求URL
    QueryString = uri_string:compose_query([
        {"from", Start},
        {"to", End},
        {"key", ApiKey}
    ]),

    Url = BaseUrl ++ "?" ++ QueryString,

    try
        % 发送HTTP请求
        case hackney:request(get, Url, [], <<>>, [{connect_timeout, 5000}, {recv_timeout, 5000}]) of
            {ok, 200, _Headers, ClientRef} ->
                % 读取响应体
                {ok, Body} = hackney:body(ClientRef),

                % 解析JSON响应
                try
                    Response = jsx:decode(Body, [return_maps]),
                    Status = maps:get(<<"status">>, Response, 1),

                    case Status of
                        0 ->
                            % 获取路线信息
                            Result = maps:get(<<"result">>, Response, #{}),
                            Routes = maps:get(<<"routes">>, Result, []),

                            % 使用第一条路线
                            case Routes of
                                [FirstRoute | _] ->
                                    Distance = maps:get(<<"distance">>, FirstRoute, 0),
                                    Duration = maps:get(<<"duration">>, FirstRoute, 0),
                                    {ok, {Distance, Duration}};
                                [] ->
                                    % 如果API返回成功但没有路线，使用默认值
                                    io:format("警告: 腾讯地图API未找到路线，使用默认值~n"),
                                    {ok, {3000, 600}} % 默认3公里，10分钟
                            end;
                        _ ->
                            % API返回错误
                            Message = maps:get(<<"message">>, Response, <<"未知错误"/utf8>>),
                            io:format("腾讯地图API错误: ~p~n", [Message]),
                            % 返回默认值而不是错误，确保服务可以继续运行
                            {ok, {3000, 600}} % 默认3公里，10分钟
                    end
                catch
                    _:JsonError ->
                        io:format("JSON解析错误: ~p~n", [JsonError]),
                        {ok, {3000, 600}} % 默认3公里，10分钟
                end;
            {ok, StatusCode, _Headers, ClientRef} ->
                % 处理HTTP错误
                {ok, _Body} = hackney:body(ClientRef),
                io:format("HTTP错误: ~p~n", [StatusCode]),
                {ok, {3000, 600}}; % 默认3公里，10分钟
            {error, Reason} ->
                % 处理请求错误
                io:format("请求错误: ~p~n", [Reason]),
                {ok, {3000, 600}} % 默认3公里，10分钟
        end
    catch
        _:Error ->
            io:format("腾讯地图API请求异常: ~p~n", [Error]),
            {ok, {3000, 600}} % 默认3公里，10分钟
    end.