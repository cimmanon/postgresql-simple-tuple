# Database.PostgreSQ.Simple.Tuple

This is a helper library for turning a chain of FromRow types from a `query` or `query_` function into tuples.  In the case of the InnerJoin and OuterJoin functions, the tuples will be converted to a tuple containing a tuple and a list (eg. `((a, b), [c])`.

# Usage

## Simple Tuple

```haskell
list :: (HasPostgres m, Functor m, FromRow a, FromRow b) => m [(a, b)]
list = map tuple2 <$> query_ "SELECT * FROM t"
```

## Inner Join

```haskell
list :: (HasPostgres m, Functor m, FromRow a, FromRow b) => m [(a, [b])]
list = join1of2 <$> query_ "SELECT t1.*, t2.* FROM t1 JOIN t2 USING (primary_key)"
```

## Outer Join

Outer joins require an extra instance for creating a `Maybe a` of whatever your `a` type is.

```haskell
{-# LANGUAGE FlexibleInstances #-}

instance FromRow Thing where
	fromRow = Thing <$> field <*> field <*> field

instance FromRow (Maybe Thing) where
	fromRow = maybeThing <$> field <*> field <*> field

maybeThing a b c =
	if isJust a && isJust b
		then Just (Thing (fromJust a) (fromJust b) c
		else Nothing

list :: (HasPostgres m, Functor m, FromRow a) => m [(a, [Thing])]
list = ojoin1of2 <$> query_ "SELECT t1.*, t2.* FROM t1 JOIN t2 USING (primary_key)"

```
