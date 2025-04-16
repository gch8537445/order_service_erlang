%%%-------------------------------------------------------------------
%%% @doc 预估价HTTP处理模块
%%% 处理打车预估价的HTTP请求
%%% @end
%%%-------------------------------------------------------------------
-module(estimate_price_handler).

-export([init/2]).

%%--------------------------------------------------------------------
%% @doc 处理HTTP请求
%% @end
%%--------------------------------------------------------------------
init(Req0, State) ->
    % 根据HTTP方法分发处理
    Method = cowboy_req:method(Req0),
    Req = handle_request(Method, Req0),
    {ok, Req, State}.

%%--------------------------------------------------------------------
%% @doc 处理不同HTTP方法的请求
%% @end
%%--------------------------------------------------------------------
handle_request(<<"POST">>, Req0) ->
    % 读取请求体
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    try
        % 解析JSON请求
        RequestData = jsx:decode(Body, [return_maps]),
        
        % 提取参数
        Start = maps:get(<<"start">>, RequestData, <<"">>),
        End = maps:get(<<"end">>, RequestData, <<"">>),
        UserId = maps:get(<<"user_id">>, RequestData, <<"">>),
        
        % 参数验证
        case validate_params(Start, End, UserId) of
            ok ->
                % 调用预估价服务，异步处理
                case estimate_service:estimate_price(Start, End, UserId) of
                    {ok, PriceEstimates} ->
                        % 返回成功响应
                        cowboy_req:reply(
                            200, 
                            #{<<"content-type">> => <<"application/json">>},
                            jsx:encode(#{
                                <<"code">> => 0,
                                <<"message">> => <<"成功">>,
                                <<"data">> => PriceEstimates
                            }),
                            Req1
                        );
                    {error, Reason} ->
                        % 返回业务错误
                        cowboy_req:reply(
                            400,
                            #{<<"content-type">> => <<"application/json">>},
                            jsx:encode(#{
                                <<"code">> => 1001,
                                <<"message">> => error_to_binary(Reason)
                            }),
                            Req1
                        )
                end;
            {error, Reason} ->
                % 返回参数验证错误
                cowboy_req:reply(
                    400,
                    #{<<"content-type">> => <<"application/json">>},
                    jsx:encode(#{
                        <<"code">> => 1000,
                        <<"message">> => Reason
                    }),
                    Req1
                )
        end
    catch
        _:_ ->
            % 处理JSON解析异常
            cowboy_req:reply(
                400,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{
                    <<"code">> => 1002,
                    <<"message">> => <<"请求参数格式错误">>
                }),
                Req1
            )
    end;

handle_request(_, Req) ->
    % 处理不支持的HTTP方法
    cowboy_req:reply(
        405,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{
            <<"code">> => 1003,
            <<"message">> => <<"不支持的请求方法">>
        }),
        Req
    ).

%%--------------------------------------------------------------------
%% @doc 验证请求参数
%% @end
%%--------------------------------------------------------------------
validate_params(<<"">>, _, _) ->
    {error, <<"起点坐标不能为空">>};
validate_params(_, <<"">>, _) ->
    {error, <<"终点坐标不能为空">>};
validate_params(_, _, <<"">>) ->
    {error, <<"用户ID不能为空">>};
validate_params(_, _, _) ->
    ok.

%%--------------------------------------------------------------------
%% @doc 将错误原因转换为二进制格式
%% @end
%%--------------------------------------------------------------------
error_to_binary(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
error_to_binary(Reason) when is_list(Reason) ->
    list_to_binary(Reason);
error_to_binary(Reason) when is_binary(Reason) ->
    Reason;
error_to_binary(_) ->
    <<"未知错误"/utf8>>.