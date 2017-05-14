{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances, TypeOperators #-}

module Database.PostgreSQL.Simple.Tuple.InnerJoin
	( join1of2
	, join1of3
	, join2of3
	, join1of4
	, join2of4
	, join3of4
	, join1of5
	, join2of5
	, join3of5
	, join4of5
	, join1of6
	, join2of6
	, join3of6
	, join4of6
	, join5of6
	, join1of7
	, join2of7
	, join3of7
	, join4of7
	, join5of7
	, join6of7
	, join1of8
	, join2of8
	, join3of8
	, join4of8
	, join5of8
	, join6of8
	, join7of8
	, join1of9
	, join2of9
	, join3of9
	, join4of9
	, join5of9
	, join6of9
	, join7of9
	, join8of9
	, join1of10
	, join2of10
	, join3of10
	, join4of10
	, join5of10
	, join6of10
	, join7of10
	, join8of10
	, join9of10
	, join1of11
	, join2of11
	, join3of11
	, join4of11
	, join5of11
	, join6of11
	, join7of11
	, join8of11
	, join9of11
	, join10of11
	)
where

import Control.Arrow (second)
import Data.Maybe (catMaybes, mapMaybe)
import Database.PostgreSQL.Simple ((:.)(..))

import Database.PostgreSQL.Simple.Tuple.Common
import Database.PostgreSQL.Simple.Tuple.Regroup

---------------------------------------------------------------------- | Groups of 2

join1of2 :: (Eq a, Eq b) =>
	[(a :. b)] -> [(a, [b])]
join1of2 xs = groupJoin $ map tuple2 xs

---------------------------------------------------------------------- | Groups of 3

join1of3 :: (Eq a, Eq b, Eq c) =>
	[(a :. (b :. c))] -> [(a, [(b, c)])]
join1of3 xs = groupJoin $ map tuple1of3 xs

join2of3 :: (Eq a, Eq b, Eq c) =>
	[(a :. (b :. c))] -> [(a, b, [c])]
join2of3 xs = map regroup3 . groupJoin $ map tuple2of3 xs

---------------------------------------------------------------------- | Groups of 4

join1of4 :: (Eq a, Eq b, Eq c, Eq d) =>
	[(a :. (b :. (c :. d)))] -> [(a, [(b, c, d)])]
join1of4 xs = groupJoin $ map tuple1of4 xs

join2of4 :: (Eq a, Eq b, Eq c, Eq d) =>
	[(a :. (b :. (c :. d)))] -> [(a, b, [(c, d)])]
join2of4 xs = map regroup3 . groupJoin $ map tuple2of4 xs

join3of4 :: (Eq a, Eq b, Eq c, Eq d) =>
	[(a :. (b :. (c :. d)))] -> [(a, b, c, [d])]
join3of4 xs = map regroup4 . groupJoin $ map tuple3of4 xs

---------------------------------------------------------------------- | Groups of 5

join1of5 :: (Eq a, Eq b, Eq c, Eq d, Eq e) =>
	[(a :. (b :. (c :. (d :. e))))] -> [(a, [(b, c, d, e)])]
join1of5 xs = groupJoin $ map tuple1of5 xs

join2of5 :: (Eq a, Eq b, Eq c, Eq d, Eq e) =>
	[(a :. (b :. (c :. (d :. e))))] -> [(a, b, [(c, d, e)])]
join2of5 xs = map regroup3 . groupJoin $ map tuple2of5 xs

join3of5 :: (Eq a, Eq b, Eq c, Eq d, Eq e) =>
	[(a :. (b :. (c :. (d :. e))))] -> [(a, b, c, [(d, e)])]
join3of5 xs = map regroup4 . groupJoin $ map tuple3of5 xs

join4of5 :: (Eq a, Eq b, Eq c, Eq d, Eq e) =>
	[(a :. (b :. (c :. (d :. e))))] -> [(a, b, c, d, [e])]
join4of5 xs = map regroup5 . groupJoin $ map tuple4of5 xs

---------------------------------------------------------------------- | Groups of 6

join1of6 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) =>
	[(a :. (b :. (c :. (d :. (e :. f)))))] -> [(a, [(b, c, d, e, f)])]
join1of6 xs = groupJoin $ map tuple1of6 xs

join2of6 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) =>
	[(a :. (b :. (c :. (d :. (e :. f)))))] -> [(a, b, [(c, d, e, f)])]
join2of6 xs = map regroup3 . groupJoin $ map tuple2of6 xs

join3of6 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) =>
	[(a :. (b :. (c :. (d :. (e :. f)))))] -> [(a, b, c, [(d, e, f)])]
join3of6 xs = map regroup4 . groupJoin $ map tuple3of6 xs

join4of6 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) =>
	[(a :. (b :. (c :. (d :. (e :. f)))))] -> [(a, b, c, d, [(e, f)])]
join4of6 xs = map regroup5 . groupJoin $ map tuple4of6 xs

join5of6 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) =>
	[(a :. (b :. (c :. (d :. (e :. f)))))] -> [(a, b, c, d, e, [f])]
join5of6 xs = map regroup6 . groupJoin $ map tuple5of6 xs

---------------------------------------------------------------------- | Groups of 7

join1of7 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. g))))))] -> [(a, [(b, c, d, e, f, g)])]
join1of7 xs = groupJoin $ map tuple1of7 xs

join2of7 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. g))))))] -> [(a, b, [(c, d, e, f, g)])]
join2of7 xs = map regroup3 . groupJoin $ map tuple2of7 xs

join3of7 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. g))))))] -> [(a, b, c, [(d, e, f, g)])]
join3of7 xs = map regroup4 . groupJoin $ map tuple3of7 xs

join4of7 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. g))))))] -> [(a, b, c, d, [(e, f, g)])]
join4of7 xs = map regroup5 . groupJoin $ map tuple4of7 xs

join5of7 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. g))))))] -> [(a, b, c, d, e, [(f, g)])]
join5of7 xs = map regroup6 . groupJoin $ map tuple5of7 xs

join6of7 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. g))))))] -> [(a, b, c, d, e, f, [g])]
join6of7 xs = map regroup7 . groupJoin $ map tuple6of7 xs

---------------------------------------------------------------------- | Groups of 8

join1of8 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. h)))))))] -> [(a, [(b, c, d, e, f, g, h)])]
join1of8 xs = groupJoin $ map tuple1of8 xs

join2of8 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. h)))))))] -> [(a, b, [(c, d, e, f, g, h)])]
join2of8 xs = map regroup3 . groupJoin $ map tuple2of8 xs

join3of8 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. h)))))))] -> [(a, b, c, [(d, e, f, g, h)])]
join3of8 xs = map regroup4 . groupJoin $ map tuple3of8 xs

join4of8 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. h)))))))] -> [(a, b, c, d, [(e, f, g, h)])]
join4of8 xs = map regroup5 . groupJoin $ map tuple4of8 xs

join5of8 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. h)))))))] -> [(a, b, c, d, e, [(f, g, h)])]
join5of8 xs = map regroup6 . groupJoin $ map tuple5of8 xs

join6of8 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. h)))))))] -> [(a, b, c, d, e, f, [(g, h)])]
join6of8 xs = map regroup7 . groupJoin $ map tuple6of8 xs

join7of8 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. h)))))))] -> [(a, b, c, d, e, f, g, [h])]
join7of8 xs = map regroup8 . groupJoin $ map tuple7of8 xs

---------------------------------------------------------------------- | Groups of 9

join1of9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. i))))))))] -> [(a, [(b, c, d, e, f, g, h, i)])]
join1of9 xs = groupJoin $ map tuple1of9 xs

join2of9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. i))))))))] -> [(a, b, [(c, d, e, f, g, h, i)])]
join2of9 xs = map regroup3 . groupJoin $ map tuple2of9 xs

join3of9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. i))))))))] -> [(a, b, c, [(d, e, f, g, h, i)])]
join3of9 xs = map regroup4 . groupJoin $ map tuple3of9 xs

join4of9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. i))))))))] -> [(a, b, c, d, [(e, f, g, h, i)])]
join4of9 xs = map regroup5 . groupJoin $ map tuple4of9 xs

join5of9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. i))))))))] -> [(a, b, c, d, e, [(f, g, h, i)])]
join5of9 xs = map regroup6 . groupJoin $ map tuple5of9 xs

join6of9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. i))))))))] -> [(a, b, c, d, e, f, [(g, h, i)])]
join6of9 xs = map regroup7 . groupJoin $ map tuple6of9 xs

join7of9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. i))))))))] -> [(a, b, c, d, e, f, g, [(h, i)])]
join7of9 xs = map regroup8 . groupJoin $ map tuple7of9 xs

join8of9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. i))))))))] -> [(a, b, c, d, e, f, g, h, [i])]
join8of9 xs = map regroup9 . groupJoin $ map tuple8of9 xs

---------------------------------------------------------------------- | Groups of 10

join1of10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. j)))))))))] -> [(a, [(b, c, d, e, f, g, h, i, j)])]
join1of10 xs = groupJoin $ map tuple1of10 xs

join2of10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. j)))))))))] -> [(a, b, [(c, d, e, f, g, h, i, j)])]
join2of10 xs = map regroup3 . groupJoin $ map tuple2of10 xs

join3of10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. j)))))))))] -> [(a, b, c, [(d, e, f, g, h, i, j)])]
join3of10 xs = map regroup4 . groupJoin $ map tuple3of10 xs

join4of10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. j)))))))))] -> [(a, b, c, d, [(e, f, g, h, i, j)])]
join4of10 xs = map regroup5 . groupJoin $ map tuple4of10 xs

join5of10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. j)))))))))] -> [(a, b, c, d, e, [(f, g, h, i, j)])]
join5of10 xs = map regroup6 . groupJoin $ map tuple5of10 xs

join6of10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. j)))))))))] -> [(a, b, c, d, e, f, [(g, h, i, j)])]
join6of10 xs = map regroup7 . groupJoin $ map tuple6of10 xs

join7of10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. j)))))))))] -> [(a, b, c, d, e, f, g, [(h, i, j)])]
join7of10 xs = map regroup8 . groupJoin $ map tuple7of10 xs

join8of10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. j)))))))))] -> [(a, b, c, d, e, f, g, h, [(i, j)])]
join8of10 xs = map regroup9 . groupJoin $ map tuple8of10 xs

join9of10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. j)))))))))] -> [(a, b, c, d, e, f, g, h, i, [j])]
join9of10 xs = map regroup10 . groupJoin $ map tuple9of10 xs

---------------------------------------------------------------------- | Groups of 11

join1of11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. k))))))))))] -> [(a, [(b, c, d, e, f, g, h, i, j, k)])]
join1of11 xs = groupJoin $ map tuple1of11 xs

join2of11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. k))))))))))] -> [(a, b, [(c, d, e, f, g, h, i, j, k)])]
join2of11 xs = map regroup3 . groupJoin $ map tuple2of11 xs

join3of11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. k))))))))))] -> [(a, b, c, [(d, e, f, g, h, i, j, k)])]
join3of11 xs = map regroup4 . groupJoin $ map tuple3of11 xs

join4of11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. k))))))))))] -> [(a, b, c, d, [(e, f, g, h, i, j, k)])]
join4of11 xs = map regroup5 . groupJoin $ map tuple4of11 xs

join5of11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. k))))))))))] -> [(a, b, c, d, e, [(f, g, h, i, j, k)])]
join5of11 xs = map regroup6 . groupJoin $ map tuple5of11 xs

join6of11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. k))))))))))] -> [(a, b, c, d, e, f, [(g, h, i, j, k)])]
join6of11 xs = map regroup7 . groupJoin $ map tuple6of11 xs

join7of11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. k))))))))))] -> [(a, b, c, d, e, f, g, [(h, i, j, k)])]
join7of11 xs = map regroup8 . groupJoin $ map tuple7of11 xs

join8of11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. k))))))))))] -> [(a, b, c, d, e, f, g, h, [(i, j, k)])]
join8of11 xs = map regroup9 . groupJoin $ map tuple8of11 xs

join9of11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. k))))))))))] -> [(a, b, c, d, e, f, g, h, i, [(j, k)])]
join9of11 xs = map regroup10 . groupJoin $ map tuple9of11 xs

join10of11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. k))))))))))] -> [(a, b, c, d, e, f, g, h, i, j, [k])]
join10of11 xs = map regroup11 . groupJoin $ map tuple10of11 xs
