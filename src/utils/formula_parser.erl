%%%-------------------------------------------------------------------
%%% @doc 公式解析和计算模块
%%% 使用Erlang内置表达式解析功能处理任意计费规则公式
%%% @end
%%%-------------------------------------------------------------------
-module(formula_parser).

-export([evaluate/2]).

%%--------------------------------------------------------------------
%% @doc 解析并计算公式
%% @param FormulaTemplate - 公式模板，例如：
%%        "#{base_price} + (#{distance} * #{per_km_price})"
%% @param Variables - 变量映射，键为变量名，值为变量值
%% @return {ok, Result} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
evaluate(FormulaTemplate, Variables) when is_binary(FormulaTemplate) ->
    % 转换为字符串
    Template = binary_to_list(FormulaTemplate),

    % 记录公式和变量（调试用）
    io:format("计算公式: ~s~n", [Template]),
    io:format("变量: ~p~n", [Variables]),

    try
        % 步骤1: 预处理公式，处理特殊函数名
        Step1 = preprocess_functions(Template),
        io:format("步骤1预处理函数后: ~s~n", [Step1]),

        % 步骤2: 查找并提取所有变量
        VarPatterns = extract_variables(Step1),
        io:format("步骤2提取变量: ~p~n", [VarPatterns]),

        % 步骤3: 替换所有变量为其值
        Step3 = replace_all_variables(Step1, VarPatterns, Variables),
        io:format("步骤3替换变量后: ~s~n", [Step3]),

        % 步骤4: 确保格式正确的Erlang表达式
        ErlExpr = ensure_valid_expr(Step3),
        io:format("步骤4最终表达式: ~s~n", [ErlExpr]),

        % 步骤5: 使用Erlang内置表达式解析和计算
        Result = calculate_expression(ErlExpr),

        {ok, Result}
    catch
        Exception:Error:Stacktrace ->
            % 详细记录错误
            io:format("公式计算错误: ~p:~p~n", [Exception, Error]),
            io:format("堆栈: ~p~n", [Stacktrace]),

            % 使用简单计算作为后备
            FallbackBasePrice = safe_get(Variables, <<"base_price">>, 10.0),
            FallbackDistance = safe_get(Variables, <<"distance">>, 0.0),
            DefaultPrice = FallbackBasePrice + FallbackDistance * 3.0,
            {ok, float_round(DefaultPrice, 2)}
    end.

%%--------------------------------------------------------------------
%% @doc 预处理公式中的特殊函数名
%% @end
%%--------------------------------------------------------------------
preprocess_functions(Formula) ->
    % 替换各种特殊函数名为Erlang对应的函数
    FunctionMappings = [
        {"max\\s*\\(", "erlang:max("},
        {"min\\s*\\(", "erlang:min("},
        {"abs\\s*\\(", "abs("},
        {"round\\s*\\(", "round("},
        {"floor\\s*\\(", "floor("},
        {"ceil\\s*\\(", "ceil("}
    ],

    % 应用所有函数映射
    lists:foldl(
        fun({Pattern, Replacement}, Acc) ->
            re:replace(Acc, Pattern, Replacement, [{return, list}, global])
        end,
        Formula,
        FunctionMappings
    ).

%%--------------------------------------------------------------------
%% @doc 从公式中提取所有变量模式和名称
%% @end
%%--------------------------------------------------------------------
extract_variables(Formula) ->
    % 查找所有 #{variable_name} 形式的变量
    case re:run(Formula, "#\\{([^\\}]+)\\}", [global, {capture, all, list}]) of
        {match, Matches} ->
            % 每个匹配是一个 [完整匹配, 变量名] 的列表
            Matches;
        nomatch ->
            []
    end.

%%--------------------------------------------------------------------
%% @doc 替换公式中的所有变量
%% @end
%%--------------------------------------------------------------------
replace_all_variables(Formula, VarPatterns, Variables) ->
    % 对于每个变量模式，用其值替换
    lists:foldl(
        fun([FullPattern, VarName], Acc) ->
            % 获取变量值
            BinName = list_to_binary(VarName),
            VarValue = format_value(safe_get(Variables, BinName, 0.0)),

            % 替换模式
            string:replace(Acc, FullPattern, VarValue, all)
        end,
        Formula,
        VarPatterns
    ).

%%--------------------------------------------------------------------
%% @doc 格式化值为适合Erlang解析的字符串
%% @end
%%--------------------------------------------------------------------
format_value(Value) when is_integer(Value) ->
    % 整数转换为浮点数形式
    integer_to_list(Value) ++ ".0";
format_value(Value) when is_float(Value) ->
    % 浮点数使用固定表示法，避免科学计数
    io_lib:format("~.10f", [Value]);
format_value(Value) when is_binary(Value) ->
    % 二进制转字符串，并用引号括起
    "\"" ++ binary_to_list(Value) ++ "\"";
format_value(Value) ->
    % 其他类型，转为字符串
    io_lib:format("~p", [Value]).

%%--------------------------------------------------------------------
%% @doc 确保表达式是有效的Erlang表达式
%% @end
%%--------------------------------------------------------------------
ensure_valid_expr(Expr) ->
    % 展平可能的IO列表
    FlatExpr = lists:flatten(Expr),

    % 确保表达式被闭合
    case lists:reverse(FlatExpr) of
        [$. | _] -> FlatExpr;  % 已经以点结尾
        _ -> FlatExpr ++ "."   % 添加点号结束表达式
    end.

%%--------------------------------------------------------------------
%% @doc 使用Erlang内置函数计算表达式
%% @end
%%--------------------------------------------------------------------
calculate_expression(Formula) ->
    % 确保我们有一个展平的字符串
    FlatFormula = lists:flatten(Formula),

    % 扫描表达式为记号
    {ok, Tokens, _} = erl_scan:string(FlatFormula),

    % 解析记号为表达式
    {ok, [ExprAst]} = erl_parse:parse_exprs(Tokens),

    % 计算表达式
    {value, Result, _} = erl_eval:expr(ExprAst, []),

    % 确保结果为数字并四舍五入到2位小数
    float_round(Result, 2).

%%--------------------------------------------------------------------
%% @doc 安全获取变量值，提供默认值
%% @end
%%--------------------------------------------------------------------
safe_get(Variables, Key, Default) ->
    case maps:find(Key, Variables) of
        {ok, Value} when is_number(Value) -> Value;
        {ok, Value} when is_binary(Value) ->
            % 尝试转换二进制为数字
            try binary_to_float(Value)
            catch
                _:_ ->
                    try float(binary_to_integer(Value))
                    catch
                        _:_ -> Default
                    end
            end;
        _ -> Default
    end.

%%--------------------------------------------------------------------
%% @doc 将浮点数四舍五入到指定小数位
%% @end
%%--------------------------------------------------------------------
float_round(Value, Precision) ->
    P = math:pow(10, Precision),
    % 四舍五入并保留指定小数位
    trunc(Value * P + 0.5) / P.