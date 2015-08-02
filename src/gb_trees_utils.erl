-module(gb_trees_utils).

-export([greatest_key_lt/2,
         greatest_key_lte/2,
         smallest_key_gt/2,
         smallest_key_gte/2
        ]).

%%
%% @doc Find the entry containing the greatest Key less than (lt) SearchKey in Tree.
%%
-spec greatest_key_lt(SearchKey::term(), Tree::gb_trees:tree()) ->
    'none' | {Key::term(), Value::term()}.

greatest_key_lt(SearchKey, {_, TreeNode}) ->
    gklt(SearchKey, TreeNode, none).

gklt(SearchKey, {Key, Value, _, LargerTree}, _) when Key < SearchKey ->
    gklt(SearchKey, LargerTree, {Key, Value});
gklt(SearchKey, {_, _, SmallerTree, _}, BestSoFar) ->
    gklt(SearchKey, SmallerTree, BestSoFar);
gklt(_, nil, BestSoFar) ->
    BestSoFar.


%%
%% @doc Find the entry containing the greatest Key less than or equal (lte) to SearchKey in Tree.
%%
-spec greatest_key_lte(SearchKey::term(), Tree::gb_trees:tree()) ->
    'none' | {Key::term(), Value::term()}.

greatest_key_lte(SearchKey, {_, TreeNode}) ->
    gklte(SearchKey, TreeNode, none).

gklte(SearchKey, {Key, Value, _, LargerTree}, _) when Key < SearchKey ->
    gklte(SearchKey, LargerTree, {Key, Value});
gklte(SearchKey, {Key, _, SmallerTree, _}, BestSoFar) when SearchKey < Key ->
    gklte(SearchKey, SmallerTree, BestSoFar);
gklte(_, {Key, Value, _, _}, _) ->
    {Key, Value};
gklte(_, nil, BestSoFar) ->
    BestSoFar.


%%
%% @doc Find the entry containing the smallest Key key greater than (gt) SearchKey in Tree.
%%
-spec smallest_key_gt(SearchKey::term(), Tree::gb_trees:tree()) ->
    'none' | {Key::term(), Value::term()}.

smallest_key_gt(SearchKey, {_, TreeNode}) ->
    skgt(SearchKey, TreeNode, none).

skgt(SearchKey, {Key, Value, SmallerTree, _}, _) when SearchKey < Key ->
    skgt(SearchKey, SmallerTree, {Key, Value});
skgt(SearchKey, {_, _, _, LargerTree}, BestSoFar) ->
    skgt(SearchKey, LargerTree, BestSoFar);
skgt(_, nil, BestSoFar) ->
    BestSoFar.


%%
%% @doc Find the entry containing the smallest Key key greater than or equal (gte) SearchKey in Tree.
%%
-spec smallest_key_gte(SearchKey::term(), Tree::gb_trees:tree()) ->
    'none' | {Key::term(), Value::term()}.

smallest_key_gte(SearchKey, {_, TreeNode}) ->
    skgte(SearchKey, TreeNode, none).

skgte(SearchKey, {Key, Value, SmallerTree, _}, _) when SearchKey < Key ->
    skgte(SearchKey, SmallerTree, {Key, Value});
skgte(SearchKey, {Key, _, _, LargerTree}, BestSoFar) when Key < SearchKey ->
    skgte(SearchKey, LargerTree, BestSoFar);
skgte(_, {Key, Value, _, _}, _) ->
    {Key, Value};
skgte(_, nil, BestSoFar) ->
    BestSoFar.

