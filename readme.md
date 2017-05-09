# Database.PostgreSQL.Simple.Tuple

This is a helper library for turning a chain of FromRow types from a `query` or `query_` function into tuples.  In addition to tuples, grouping functions are provided to compress results into a tuple containing a tuple and a list (eg. `((a, b), [c])`.  Note that this requires Eq instances on all of the FromRow types that appear in the first tuple.

This library is heavily inspired by the [O/R Broker](https://github.com/nilskp/orbroker) library written in Scala (see also: [1](https://code.google.com/archive/p/orbroker/) and [2](https://code.google.com/archive/p/orbroker/wikis/JoinExample.wiki)).

# Usage

## Simple Tuple

```haskell
list :: (HasPostgres m, Functor m, Eq a, FromRow a, FromRow b) => m [(a, b)]
list = map tuple2 <$> query_ "SELECT * FROM t"
```

## Group Joining (Inner Join)

```haskell
list :: (HasPostgres m, Functor m, Eq a, FromRow a, FromRow b) => m [(a, [b])]
list = join1of2 <$> query_ "SELECT t1.*, t2.* FROM t1 JOIN t2 USING (primary_key)"
```

## Group Joining (Outer Join)

Outer joins require an extra instance for creating a `Maybe a` of whatever your `a` type is.

```haskell
{-# LANGUAGE FlexibleInstances #-}

instance FromRow Thing where
	fromRow = Thing <$> field <*> field <*> field

instance FromRow (Maybe Thing) where
	fromRow = maybeThing <$> field <*> field <*> field

maybeThing (Just a) (Just b) c = Just $ Thing a b c
maybeThing _ _ _ = Nothing

list :: (HasPostgres m, Functor m, Eq a, FromRow a) => m [(a, [Thing])]
list = ojoin1of2 <$> query_ "SELECT t1.*, t2.* FROM t1 LEFT JOIN t2 USING (primary_key)"

```
