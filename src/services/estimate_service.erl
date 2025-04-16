%%%-------------------------------------------------------------------
%%% @doc 预估价服务模块
%%% 提供预估价计算的核心业务逻辑
%%% @end
%%%-------------------------------------------------------------------
-module(estimate_service).

-export([estimate_price/3]).

%%--------------------------------------------------------------------
%% @doc 计算预估价
%% @param Start - 起点坐标 (格式: "纬度,经度")
%% @param End - 终点坐标 (格式: "纬度,经度")
%% @param UserId - 用户ID
%% @return {ok, PriceEstimates} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
estimate_price(Start, End, UserId) when is_binary(Start), is_binary(End), is_binary(UserId) ->
    % 转换坐标格式
    StartStr = binary_to_list(Start),
    EndStr = binary_to_list(End),
    % UserId目前未在计算中使用，但保留参数以便将来扩展
    % 例如可用于个性化定价、会员折扣等

    % 启动一个计时器，用于记录处理时间
    StartTime = erlang:system_time(millisecond),

    try
        % 并行获取行程信息和计费规则
        % 任务1: 获取行程距离和时间
        RouteInfoFuture = spawn_task(fun() ->
            tencent_map_api:get_route_info(StartStr, EndStr)
                                     end),

        % 任务2: 获取所有计费规则
        RulesFuture = spawn_task(fun() ->
            pricing_rules_repo:get_all_pricing_rules()
                                 end),

        % 等待并获取任务结果
        {ok, {Distance, Duration}} = collect_task_result(RouteInfoFuture),
        {ok, Rules} = collect_task_result(RulesFuture),

        % 当前时间，用于计算是否为夜间
        CurrentTime = calendar:universal_time(),

        % 为每个计费规则计算预估价
        PriceEstimates = lists:map(
            fun(Rule) ->
                % 解析规则信息

                RuleId = maps:get(<<"id">>, Rule),
                VehicleType = maps:get(<<"vehicle_type">>, Rule),
                IpathTransCode = maps:get(<<"ipath_trans_code">>, Rule),
                RuleName = maps:get(<<"rule_name">>, Rule),
                FormulaTemplate = maps:get(<<"formula_template">>, Rule),

                % 获取费用项
                FeeItemsResult = pricing_rules_repo:get_pricing_rule_items(RuleId),
                FeeItems = case FeeItemsResult of
                               {ok, Items} -> Items;
                               _ -> [] % 如果获取费用项失败，使用空列表
                           end,

                % 创建变量映射
                BaseVariables = #{
                    <<"distance">> => Distance / 1000,  % 千米
                    <<"duration">> => Duration / 60     % 分钟
                },
                % 将费用项添加到变量映射中
                Variables = add_fee_items_to_variables(FeeItems, BaseVariables, CurrentTime),
                io:format("Rule: ~p~n", [Rule]),
                % 解析并计算价格，处理可能的错误
                Price = case formula_parser:evaluate(FormulaTemplate, Variables) of
                            {ok, CalculatedPrice} -> CalculatedPrice;
                            _ ->
                                % 如果计算失败，使用简单公式估算
                                BasePrice = maps:get(<<"base_price">>, Variables, 10.0),
                                DistanceKm = Distance / 1000,
                                BasePrice + DistanceKm * 3.0 % 默认每公里3元
                        end,

                % 返回预估价信息
                #{
                    <<"vehicle_type">> => VehicleType,
                    <<"ipath_trans_code">> => IpathTransCode,
                    <<"rule_name">> => RuleName,
                    <<"estimated_price">> => float_round(Price, 2), % 确保价格正确格式化
                    <<"distance">> => Distance / 1000,
                    <<"duration">> => Duration / 60
                }
            end,
            Rules
        ),

        % 记录处理时间
        EndTime = erlang:system_time(millisecond),
        ProcessTime = EndTime - StartTime,
        io:format("预估价计算完成，耗时: ~p ms~n", [ProcessTime]),

        {ok, PriceEstimates}

    catch
        throw:{error, Reason} ->
            {error, Reason};
        error:Error ->
            error_logger:error_msg("预估价计算错误: ~p~n", [Error]),
            {error, <<"服务器内部错误"/utf8>>}
    end.

%%--------------------------------------------------------------------
%% @doc 异步执行任务并返回future
%% @end
%%--------------------------------------------------------------------
spawn_task(Fun) ->
    Self = self(),
    Ref = make_ref(),
    Pid = spawn(fun() ->
        try
            Result = Fun(),
            Self ! {Ref, {ok, Result}}
        catch
            E:R:S ->
                Self ! {Ref, {error, {E, R, S}}}
        end
                end),
    {Pid, Ref}.

%%--------------------------------------------------------------------
%% @doc 收集任务结果，处理可能的错误
%% @end
%%--------------------------------------------------------------------
collect_task_result({_Pid, Ref}) ->
    receive
        {Ref, {ok, {ok, Result}}} ->
            {ok, Result};
        {Ref, {ok, {error, Reason}}} ->
            throw({error, Reason});
        {Ref, {error, {_, Reason, _}}} ->
            throw({error, Reason})
    after 10000 ->
        throw({error, timeout})
    end.

%%--------------------------------------------------------------------
%% @doc 将费用项添加到变量映射
%% @end
%%--------------------------------------------------------------------
add_fee_items_to_variables(FeeItems, BaseVariables, CurrentTime) ->
    lists:foldl(
        fun(FeeItem, Acc) ->
            FeeName = maps:get(<<"fee_name">>, FeeItem),
            FeeValue = binary_to_float(maps:get(<<"fee_value">>, FeeItem)),
            TimeStart = maps:get(<<"time_start">>, FeeItem, undefined),
            TimeEnd = maps:get(<<"time_end">>, FeeItem, undefined),
            % 如果是夜间费用且当前不是夜间时段，设置为0
            ActualValue = case {FeeName, TimeStart, TimeEnd} of
                              {<<"night_fee">>, TimeStart, TimeEnd} when TimeStart /= undefined, TimeEnd /= undefined ->
                                  case is_time_in_range(CurrentTime, TimeStart, TimeEnd) of
                                      true -> FeeValue;
                                      false -> 0.0
                                  end;
                              _ ->
                                  FeeValue
                          end,
            % 将费用项添加到变量映射
            maps:put(FeeName, ActualValue, Acc)
        end,
        BaseVariables,
        FeeItems
    ).

float_round(Value, Precision) ->
    P = math:pow(10, Precision),
    % 四舍五入并保留指定小数位
    trunc(Value * P + 0.5) / P.

% 检查当前时间是否在指定范围内
is_time_in_range(CurrentTime, StartTimeBin, EndTimeBin) ->
    % 从ISO 8601格式提取时和分
    StartTimeOfDay = extract_time_of_day(StartTimeBin),
    EndTimeOfDay = extract_time_of_day(EndTimeBin),
    CurrentTimeOfDay = get_current_time_of_day(CurrentTime),

    % 比较时间（考虑跨天的情况）
    is_between_times(CurrentTimeOfDay, StartTimeOfDay, EndTimeOfDay).

% 从ISO时间戳提取时间部分（小时和分钟，转换为分钟计数）
extract_time_of_day(TimeBin) when is_binary(TimeBin) ->
    case binary:match(TimeBin, <<"T">>) of
        {Pos, _} ->
            % 提取T后面的时间部分
            [HourBin, MinBin | _] = binary:split(binary:part(TimeBin, Pos+1, 5), <<":">>, [global]),
            (binary_to_integer(HourBin) * 60) + binary_to_integer(MinBin);
        _ ->
            0  % 默认值
    end;
extract_time_of_day(_) ->
    0.  % 无效输入的默认值

% 获取当前时间的小时和分钟（转换为分钟计数）
get_current_time_of_day({{_, _, _}, {Hour, Minute, _}}) ->
    (Hour * 60) + Minute;
get_current_time_of_day(_) ->
    % 使用系统时间作为后备
    {_, {Hour, Minute, _}} = calendar:local_time(),
    (Hour * 60) + Minute.

% 检查时间是否在范围内（处理跨天的情况）
is_between_times(Current, Start, End) ->
    if
        Start =< End ->
            % 同一天内的时间范围
            (Current >= Start) andalso (Current =< End);
        true ->
            % 跨天的时间范围（如23:00到次日06:00）
            (Current >= Start) orelse (Current =< End)
    end.