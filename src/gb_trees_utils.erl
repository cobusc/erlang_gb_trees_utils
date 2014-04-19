-module(gb_trees_utils).

-export([greatest_key_lt/2,
         greatest_key_lte/2,
         smallest_key_gt/2,
         smallest_key_gte/2
        ]).

%%
%% @doc Find the entry containing the greatest Key less than (lt) SearchKey in Tree.
%%
-spec greatest_key_lt(SearchKey::term(), Tree::gb_tree()) -> 
    'none' | {Key::term(), Value::term()}.

greatest_key_lt(SearchKey, {_, TreeNode}) ->
    gklt(SearchKey, TreeNode).

gklt(SearchKey, {Key, Value, _, LargerTree}) when Key < SearchKey ->
    % Check if a more optimal solution exists.
    case gklt(SearchKey, LargerTree) of
        none -> {Key, Value};
        Better -> Better
    end;
gklt(SearchKey, {_, _, SmallerTree, _}) ->
    gklt(SearchKey, SmallerTree);
gklt(_, nil) ->
    none.


%%
%% @doc Find the entry containing the greatest Key less than or equal (lte) to SearchKey in Tree.
%%
-spec greatest_key_lte(SearchKey::term(), Tree::gb_tree()) -> 
    'none' | {Key::term(), Value::term()}.

greatest_key_lte(SearchKey, {_, TreeNode}) ->
    gklte(SearchKey, TreeNode).

gklte(SearchKey, {Key, Value, _, LargerTree}) when Key < SearchKey ->
    % Check if a more optimal solution exists.
    case gklte(SearchKey, LargerTree) of
        none -> {Key, Value};
        Better -> Better
    end;
gklte(SearchKey, {Key, _, SmallerTree, _}) when SearchKey < Key ->
    gklte(SearchKey, SmallerTree);
gklte(SearchKey, {SearchKey, Value, _, _}) ->
    {SearchKey, Value};
gklte(_, nil) ->
    none.


%%
%% @doc Find the entry containing the smallest Key key greater than (gt) SearchKey in Tree.
%%
-spec smallest_key_gt(SearchKey::term(), Tree::gb_tree()) -> 
    'none' | {Key::term(), Value::term()}.

smallest_key_gt(SearchKey, {_, TreeNode}) ->
    skgt(SearchKey, TreeNode).

skgt(SearchKey, {Key, Value, SmallerTree, _}) when SearchKey < Key ->
    % Check if a more optimal solution exists.
    case skgt(SearchKey, SmallerTree) of
        none -> {Key, Value};
        Better -> Better
    end;
skgt(SearchKey, {_, _, _, LargerTree}) ->
    skgt(SearchKey, LargerTree);
skgt(_, nil) ->
    none.


%%
%% @doc Find the entry containing the smallest Key key greater than or equal (gte) SearchKey in Tree.
%%
-spec smallest_key_gte(SearchKey::term(), Tree::gb_tree()) -> 
    'none' | {Key::term(), Value::term()}.

smallest_key_gte(SearchKey, {_, TreeNode}) ->
    skgte(SearchKey, TreeNode).

skgte(SearchKey, {Key, Value, SmallerTree, _}) when SearchKey < Key ->
    % Check if a more optimal solution exists.
    case skgte(SearchKey, SmallerTree) of
        none -> {Key, Value};
        Better -> Better
    end;
skgte(SearchKey, {Key, _, _, LargerTree}) when Key < SearchKey ->
    skgte(SearchKey, LargerTree);
skgte(SearchKey, {SearchKey, Value, _, _}) ->
    {SearchKey, Value};
skgte(_, nil) ->
    none.

