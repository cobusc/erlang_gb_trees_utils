-module(gb_trees_utils_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_TREE, gb_trees:from_orddict([{1, a}, {2, b}, {3, c}, {4, d}, {5, e}])).


%%%
%%% List-based implementations [O(n)] to test the tree-based implementation [O(log n)] against
%%%

greatest_key_lt(SearchKey, [{Key, Value}  | Rest]) when Key < SearchKey ->
    case greatest_key_lt(SearchKey, Rest) of
        none -> {Key, Value};
        Better -> Better
    end;
greatest_key_lt(SearchKey, [{Key, _}  | _]) when SearchKey < Key ->
    none;
greatest_key_lt(_, []) ->
    none.


greatest_key_lte(SearchKey, [{Key, Value}  | Rest]) when Key < SearchKey ->
    case greatest_key_lte(SearchKey, Rest) of
        none -> {Key, Value};
        Better -> Better
    end;
greatest_key_lte(SearchKey, [{Key, _}  | _]) when SearchKey < Key ->
    none;
greatest_key_lte(SearchKey, [{SearchKey, Value}  | _]) ->
    {SearchKey, Value};
greatest_key_lte(_, []) ->
    none.


smallest_key_gt(SearchKey, [{Key, Value}  | _]) when SearchKey < Key ->
    {Key, Value};
smallest_key_gt(SearchKey, [_  | Rest]) ->
    smallest_key_gt(SearchKey, Rest);
smallest_key_gt(_, []) ->
    none.


smallest_key_gte(SearchKey, [{Key, Value}  | _]) when SearchKey < Key ->
    {Key, Value};
smallest_key_gte(SearchKey, [{SearchKey, Value}  | _]) ->
    {SearchKey, Value};
smallest_key_gte(SearchKey, [_  | Rest]) ->
    smallest_key_gte(SearchKey, Rest);
smallest_key_gte(_, []) ->
    none.

%%%
%%% The tests
%%%

%%
%% Compare the gb_tree_utils functions against the list-based implementations
%%
general_test() ->
    random:seed(os:timestamp()),
    GenFun = fun(V) ->
        X = random:uniform(),
        {X, integer_to_list(V)}
    end,
    TestList = lists:usort([ GenFun(V) || V <- lists:seq(1, 1000) ]),
    TestTree = gb_trees:from_orddict(orddict:from_list(TestList)),

    TestFun = fun(SearchKey) ->
        ?assertEqual(greatest_key_lt(SearchKey, TestList), gb_trees_utils:greatest_key_lt(SearchKey, TestTree)),
        ?assertEqual(greatest_key_lte(SearchKey, TestList), gb_trees_utils:greatest_key_lte(SearchKey, TestTree)),
        ?assertEqual(smallest_key_gt(SearchKey, TestList), gb_trees_utils:smallest_key_gt(SearchKey, TestTree)),
        ?assertEqual(smallest_key_gte(SearchKey, TestList), gb_trees_utils:smallest_key_gte(SearchKey, TestTree))
    end,
    [ TestFun(random:uniform()) || _ <- lists:seq(1, 2000)],
    % Corner cases
    ?assertEqual(none, gb_trees_utils:greatest_key_lte(-1, TestTree)),
    TestFun(-1),
    ?assertEqual(none, gb_trees_utils:smallest_key_gte(2, TestTree)),
    TestFun(2).


greatest_key_lt_test() ->
    ?assertEqual(none, gb_trees_utils:greatest_key_lt(1, ?TEST_TREE)),
    ?assertEqual({1, a}, gb_trees_utils:greatest_key_lt(2, ?TEST_TREE)),
    ?assertEqual({5, e}, gb_trees_utils:greatest_key_lt(10, ?TEST_TREE)).

greatest_key_lte_test() ->
    ?assertEqual(none, gb_trees_utils:greatest_key_lte(0, ?TEST_TREE)),
    ?assertEqual({2, b}, gb_trees_utils:greatest_key_lte(2, ?TEST_TREE)),
    ?assertEqual({5, e}, gb_trees_utils:greatest_key_lte(10, ?TEST_TREE)).

smallest_key_gt_test() ->
    ?assertEqual({1, a}, gb_trees_utils:smallest_key_gt(0, ?TEST_TREE)),
    ?assertEqual({3, c}, gb_trees_utils:smallest_key_gt(2, ?TEST_TREE)),
    ?assertEqual(none, gb_trees_utils:smallest_key_gt(6, ?TEST_TREE)).

smallest_key_gte_test() ->
    ?assertEqual({2, b}, gb_trees_utils:smallest_key_gte(2, ?TEST_TREE)),
    ?assertEqual({1, a}, gb_trees_utils:smallest_key_gte(0, ?TEST_TREE)),
    ?assertEqual(none, gb_trees_utils:smallest_key_gte(6, ?TEST_TREE)).

