module Database.PostgreSQL.Simple.Tuple.Regroup where

regroup3 :: ((a, b), c) -> (a, b, c)
regroup3 ((a, b), c) = (a, b, c)

regroup4 :: ((a, b, c), d) -> (a, b, c, d)
regroup4 ((a, b, c), d) = (a, b, c, d)

regroup5 :: ((a, b, c, d), e) -> (a, b, c, d, e)
regroup5 ((a, b, c, d), e) = (a, b, c, d , e)

regroup6 :: ((a, b, c, d, e), f) -> (a, b, c, d, e, f)
regroup6 ((a, b, c, d, e), f) = (a, b, c, d , e, f)

regroup7 :: ((a, b, c, d, e, f), g) -> (a, b, c, d, e, f, g)
regroup7 ((a, b, c, d, e, f), g) = (a, b, c, d , e, f, g)

regroup8 :: ((a, b, c, d, e, f, g), h) -> (a, b, c, d, e, f, g, h)
regroup8 ((a, b, c, d, e, f, g), h) = (a, b, c, d , e, f, g, h)

regroup9 :: ((a, b, c, d, e, f, g, h), i) -> (a, b, c, d, e, f, g, h, i)
regroup9 ((a, b, c, d, e, f, g, h), i) = (a, b, c, d , e, f, g, h, i)

regroup10 :: ((a, b, c, d, e, f, g, h, i), j) -> (a, b, c, d, e, f, g, h, i, j)
regroup10 ((a, b, c, d, e, f, g, h, i), j) = (a, b, c, d , e, f, g, h, i, j)

regroup11 :: ((a, b, c, d, e, f, g, h, i, j), k) -> (a, b, c, d, e, f, g, h, i, j, k)
regroup11 ((a, b, c, d, e, f, g, h, i, j), k) = (a, b, c, d , e, f, g, h, i, j, k)
