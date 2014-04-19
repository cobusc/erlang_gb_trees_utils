-module(gb_trees_utils_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_TREE, gb_trees:from_orddict([{1, a}, {2, b}, {3, c}, {4, d}, {5, e}])).

greatest_key_lt_test() ->
    ?assertEqual({1, a}, gb_trees_utils:greatest_key_lt(2, ?TEST_TREE)),
    ?assertEqual(none, gb_trees_utils:greatest_key_lt(1, ?TEST_TREE)).

greatest_key_lte_test() ->
    ?assertEqual({2, b}, gb_trees_utils:greatest_key_lte(2, ?TEST_TREE)),
    ?assertEqual({5, e}, gb_trees_utils:greatest_key_lte(10, ?TEST_TREE)),
    ?assertEqual(none, gb_trees_utils:greatest_key_lte(0, ?TEST_TREE)).

smallest_key_gt_test() ->
    ?assertEqual({3, c}, gb_trees_utils:smallest_key_gt(2, ?TEST_TREE)),
    ?assertEqual(none, gb_trees_utils:smallest_key_gt(6, ?TEST_TREE)).

smallest_key_gte_test() ->
    ?assertEqual({2, b}, gb_trees_utils:smallest_key_gte(2, ?TEST_TREE)),
    ?assertEqual({1, a}, gb_trees_utils:smallest_key_gte(0, ?TEST_TREE)),
    ?assertEqual(none, gb_trees_utils:smallest_key_gte(6, ?TEST_TREE)).

