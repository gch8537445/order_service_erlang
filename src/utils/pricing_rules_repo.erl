%%%-------------------------------------------------------------------
%%% @doc 计费规则仓库模块
%%% 负责从数据库和缓存中获取计费规则数据
%%% @end
%%%-------------------------------------------------------------------
-module(pricing_rules_repo).

-export([
    get_all_pricing_rules/0,
    get_pricing_rule_items/1
]).

%%--------------------------------------------------------------------
%% @doc 获取所有计费规则
%% @return {ok, Rules} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
get_all_pricing_rules() ->
    % 尝试从Redis缓存获取
    case get_rules_from_cache() of
        {ok, Rules} ->
            {ok, Rules};
        {error, _} ->
            % 缓存未命中或错误，从数据库获取
            case get_rules_from_db() of
                {ok, Rules} ->
                    % 更新缓存
                    _ = set_rules_to_cache(Rules),
                    {ok, Rules};
                {error, _DbError} ->
                    % 数据库失败时返回默认规则，确保服务可用性
                    io:format("从数据库获取规则失败，使用默认规则~n"),
                    {ok, get_default_rules()}
            end
    end.

%%--------------------------------------------------------------------
%% @doc 获取特定计费规则的费用项
%% @param RuleId - 计费规则ID
%% @return {ok, FeeItems} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
get_pricing_rule_items(RuleId) ->
    % 尝试从Redis缓存获取
    CacheKey = "rule_items:" ++ integer_to_list(RuleId),
    case redis_utils:get(CacheKey) of
        {ok, Data} when is_binary(Data), byte_size(Data) > 0 ->
            % 缓存命中，解析JSON数据
            try
                {ok, jsx:decode(Data, [return_maps])}
            catch
                _:_ ->
                    % JSON解析失败，从数据库获取
                    get_rule_items_from_db_or_default(RuleId)
            end;
        _ ->
            % 缓存未命中，从数据库获取
            get_rule_items_from_db_or_default(RuleId)
    end.

%%--------------------------------------------------------------------
%% @doc 从数据库获取规则项或使用默认值
%% @end
%%--------------------------------------------------------------------
get_rule_items_from_db_or_default(RuleId) ->
    case get_rule_items_from_db(RuleId) of
        {ok, Items} ->
            % 更新缓存
            CacheKey = "rule_items:" ++ integer_to_list(RuleId),
            _ = redis_utils:set(CacheKey, jsx:encode(Items)),
            {ok, Items};
        {error, _} ->
            % 返回默认规则项
            {ok, get_default_rule_items(RuleId)}
    end.

%%--------------------------------------------------------------------
%% @doc 从Redis缓存获取计费规则
%% @end
%%--------------------------------------------------------------------
get_rules_from_cache() ->
    case redis_utils:get("all_pricing_rules") of
        {ok, Data} when is_binary(Data), byte_size(Data) > 0 ->
            % 解析JSON数据
            try
                {ok, jsx:decode(Data, [return_maps])}
            catch
                _:_ ->
                    {error, json_parse_error}
            end;
        _ ->
            {error, cache_miss}
    end.

%%--------------------------------------------------------------------
%% @doc 将计费规则存入Redis缓存
%% @end
%%--------------------------------------------------------------------
set_rules_to_cache(Rules) ->
    % 将规则转换为JSON并存入Redis，设置过期时间为1小时
    try
        redis_utils:setex("all_pricing_rules", 3600, jsx:encode(Rules))
    catch
        _:_ ->
            {error, cache_set_failed}
    end.

%%--------------------------------------------------------------------
%% @doc 从数据库获取所有计费规则
%% @end
%%--------------------------------------------------------------------
get_rules_from_db() ->
    % SQL查询
    Query = "SELECT id, vehicle_type, ipath_trans_code, rule_name, formula_template, description FROM pricing_rules",

    % 执行查询
    try
        case db_utils:query(Query, []) of
            {ok, Columns, Rows} ->
                % 将行数据转换为映射
                Rules = [row_to_map(Columns, Row) || Row <- Rows],

                {ok, Rules};
            Error ->
                Error
        end
    catch
        _:_ ->
            {error, db_query_failed}
    end.

%%--------------------------------------------------------------------
%% @doc 从数据库获取特定计费规则的费用项
%% @end
%%--------------------------------------------------------------------
get_rule_items_from_db(RuleId) ->
    % SQL查询
    Query = "SELECT id, rule_id, fee_name, fee_value, time_start, time_end, description FROM pricing_rule_items WHERE rule_id = $1",

    % 执行查询
    try
        case db_utils:query(Query, [RuleId]) of
            {ok, Columns, Rows} ->
                % 将行数据转换为映射
                Items = [row_to_map(Columns, Row) || Row <- Rows],
                {ok, Items};
            Error ->
                Error
        end
    catch
        _:_ ->
            {error, db_query_failed}
    end.

%%--------------------------------------------------------------------
%% @doc 将数据库行转换为映射
%% @end
%%--------------------------------------------------------------------
row_to_map(Columns, Row) ->
    % 更安全地提取列名，适应可能的结构变化
    ColumnNames = [element(2, Col) || Col <- Columns],

    % 确保行数据是列表
    RowList = if
                  is_tuple(Row) -> tuple_to_list(Row);
                  is_list(Row) -> Row
              end,

    % 转换并创建映射
    maps:from_list(lists:zip([Col || Col <- ColumnNames], RowList)).

%%--------------------------------------------------------------------
%% @doc 获取默认规则（数据库不可用时使用）
%% @end
%%--------------------------------------------------------------------
get_default_rules() ->
    [
        #{
            id => 1,
            vehicle_type => 1,
            ipath_trans_code => 11101,
            rule_name => <<"经济型默认计费规则"/utf8>>,
            formula_template => <<"#{base_price} + (#{distance} * #{per_km_price}) + (#{duration} * #{per_minute_price}) + #{night_fee}"/utf8>>,
            description => <<"经济型车基础计费规则"/utf8>>
        },
        #{
            id => 2,
            vehicle_type => 2,
            ipath_trans_code => 22101,
            rule_name => <<"舒适型默认计费规则"/utf8>>,
            formula_template => <<"#{base_price} + (#{distance} * #{per_km_price}) + (#{duration} * #{per_minute_price}) + #{night_fee}"/utf8>>,
            description => <<"舒适型车基础计费规则"/utf8>>
        },
        #{
            id => 3,
            vehicle_type => 3,
            ipath_trans_code => 32201,
            rule_name => <<"拼车默认计费规则"/utf8>>,
            formula_template => <<"max(#{base_price}, #{distance} * #{per_km_price} / 2)"/utf8>>,
            description => <<"拼车特殊计费规则"/utf8>>
        }
    ].

%%--------------------------------------------------------------------
%% @doc 获取默认规则项（数据库不可用时使用）
%% @end
%%--------------------------------------------------------------------
get_default_rule_items(RuleId) ->
    case RuleId of
        1 ->
            [
                #{
                    id => 1,
                    rule_id => 1,
                    fee_name => <<"base_price">>,
                    fee_value => 10.0,
                    time_start => null,
                    time_end => null,
                    description => <<"经济型起步价"/utf8>>
                },
                #{
                    id => 2,
                    rule_id => 1,
                    fee_name => <<"per_km_price">>,
                    fee_value => 2.5,
                    time_start => null,
                    time_end => null,
                    description => <<"经济型每公里费用"/utf8>>
                },
                #{
                    id => 3,
                    rule_id => 1,
                    fee_name => <<"per_minute_price">>,
                    fee_value => 0.5,
                    time_start => null,
                    time_end => null,
                    description => <<"经济型每分钟费用"/utf8>>
                },
                #{
                    id => 4,
                    rule_id => 1,
                    fee_name => <<"night_fee">>,
                    fee_value => 5.0,
                    time_start => <<"23:00:00">>,
                    time_end => <<"05:00:00">>,
                    description => <<"经济型夜间服务费"/utf8>>
                }
            ];
        2 ->
            [
                #{
                    id => 5,
                    rule_id => 2,
                    fee_name => <<"base_price">>,
                    fee_value => 15.0,
                    time_start => null,
                    time_end => null,
                    description => <<"舒适型起步价"/utf8>>
                },
                #{
                    id => 6,
                    rule_id => 2,
                    fee_name => <<"per_km_price">>,
                    fee_value => 3.0,
                    time_start => null,
                    time_end => null,
                    description => <<"舒适型每公里费用"/utf8>>
                },
                #{
                    id => 7,
                    rule_id => 2,
                    fee_name => <<"per_minute_price">>,
                    fee_value => 0.6,
                    time_start => null,
                    time_end => null,
                    description => <<"舒适型每分钟费用"/utf8>>
                },
                #{
                    id => 8,
                    rule_id => 2,
                    fee_name => <<"night_fee">>,
                    fee_value => 8.0,
                    time_start => <<"23:00:00">>,
                    time_end => <<"05:00:00">>,
                    description => <<"舒适型夜间服务费"/utf8>>
                }
            ];
        3 ->
            [
                #{
                    id => 9,
                    rule_id => 3,
                    fee_name => <<"base_price">>,
                    fee_value => 8.0,
                    time_start => null,
                    time_end => null,
                    description => <<"拼车起步价"/utf8>>
                },
                #{
                    id => 10,
                    rule_id => 3,
                    fee_name => <<"per_km_price">>,
                    fee_value => 2.0,
                    time_start => null,
                    time_end => null,
                    description => <<"拼车每公里费用"/utf8>>
                }
            ];
        _ ->
            % 对于未知规则ID，返回通用规则项
            [
                #{
                    id => 1,
                    rule_id => RuleId,
                    fee_name => <<"base_price">>,
                    fee_value => 10.0,
                    time_start => null,
                    time_end => null,
                    description => <<"通用起步价"/utf8>>
                },
                #{
                    id => 2,
                    rule_id => RuleId,
                    fee_name => <<"per_km_price">>,
                    fee_value => 2.5,
                    time_start => null,
                    time_end => null,
                    description => <<"通用每公里费用"/utf8>>
                }
            ]
    end.