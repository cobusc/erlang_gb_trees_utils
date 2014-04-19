[![Build Status](https://travis-ci.org/cobusc/erlang_gb_trees_utils.png?branch=master)](https://www.travis-ci.org/cobusc/erlang_gb_trees_utils)
Utility functions for Erlang's gb_trees
=======================================

The following functions are provided:

greatest_key_lt/2
-----------------

Find the entry containing the greatest Key less than (lt) SearchKey in Tree.

```erlang
-spec greatest_key_lt(SearchKey::term(), Tree::gb_tree()) ->
    'none' | {Key::term(), Value::term()}.
```

greatest_key_lte/2
------------------

Find the entry containing the greatest Key less than or equal (lte) to SearchKey in Tree.

```erlang
-spec greatest_key_lte(SearchKey::term(), Tree::gb_tree()) ->
    'none' | {Key::term(), Value::term()}.
```

smallest_key_gt/2
-----------------

Find the entry containing the smallest Key key greater than (gt) SearchKey in Tree.

```erlang
-spec smallest_key_gt(SearchKey::term(), Tree::gb_tree()) ->
    'none' | {Key::term(), Value::term()}.
```

smallest_key_gte/2 
------------------

Find the entry containing the smallest Key key greater than or equal (gte) to SearchKey in Tree.

```erlang
-spec smallest_key_gte(SearchKey::term(), Tree::gb_tree()) ->
    'none' | {Key::term(), Value::term()}.
```

TODO
----
Maybe add some beafier tests.

