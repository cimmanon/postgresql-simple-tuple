{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances, TypeOperators #-}

module Database.PostgreSQL.Simple.Tuple.OuterJoin
	( ojoin1of2
	, ojoin1of3
	, ojoin2of3
	, ojoin1of4
	, ojoin2of4
	, ojoin3of4
	, ojoin1of5
	, ojoin2of5
	, ojoin3of5
	, ojoin4of5
	, ojoin1of6
	, ojoin2of6
	, ojoin3of6
	, ojoin4of6
	, ojoin5of6
	, ojoin1of7
	, ojoin2of7
	, ojoin3of7
	, ojoin4of7
	, ojoin5of7
	, ojoin6of7
	, ojoin1of8
	, ojoin2of8
	, ojoin3of8
	, ojoin4of8
	, ojoin5of8
	, ojoin6of8
	, ojoin7of8
	, ojoin1of9
	, ojoin2of9
	, ojoin3of9
	, ojoin4of9
	, ojoin5of9
	, ojoin6of9
	, ojoin7of9
	, ojoin8of9
	, ojoin1of10
	, ojoin2of10
	, ojoin3of10
	, ojoin4of10
	, ojoin5of10
	, ojoin6of10
	, ojoin7of10
	, ojoin8of10
	, ojoin9of10
	, ojoin1of11
	, ojoin2of11
	, ojoin3of11
	, ojoin4of11
	, ojoin5of11
	, ojoin6of11
	, ojoin7of11
	, ojoin8of11
	, ojoin9of11
	, ojoin10of11
	)
where

import Control.Arrow (second)
import Data.Maybe (catMaybes, mapMaybe, isJust)
import Data.List (groupBy)
import Data.Function (on)
import Data.Tuple.Sequence (sequenceT)
--import Language.Haskell.TH
--import Language.Haskell.TH.Quote
import Database.PostgreSQL.Simple ((:.)(..))

import Database.PostgreSQL.Simple.Tuple.Common
import Database.PostgreSQL.Simple.Tuple.Regroup

{----------------------------------------------------------------------------------------------------{
                                                                       | Grouped Outer Joins
}----------------------------------------------------------------------------------------------------}

---------------------------------------------------------------------- | Groups of 2

ojoin1of2 :: (Eq a, Eq b) =>
	[(a :. Maybe b)] ->
	[(a, [b])]
ojoin1of2 xs = map (second catMaybes) $ groupJoin $ map tuple2 xs

---------------------------------------------------------------------- | Groups of 3

ojoin1of3 :: (Eq a, Eq b, Eq c) =>
	[(a :. (Maybe b :. Maybe c))] ->
	[(a, [(b, c)])]
ojoin1of3 xs = map (second (mapMaybe sequenceT)) $ groupJoin $ map tuple1of3 xs

ojoin2of3 :: (Eq a, Eq b, Eq c) =>
	[(a :. (b :. Maybe c))] ->
	[(a, b, [c])]
ojoin2of3 xs = map (regroup3 . second catMaybes) $ groupJoin $ map tuple2of3 xs

---------------------------------------------------------------------- | Groups of 4

ojoin1of4 :: (Eq a, Eq b, Eq c, Eq d) =>
	[(a :. (Maybe b :. (Maybe c :. Maybe d)))] ->
	[(a, [(b, c, d)])]
ojoin1of4 xs = map (second (mapMaybe sequenceT)) $ groupJoin $ map tuple1of4 xs

ojoin2of4 :: (Eq a, Eq b, Eq c, Eq d) =>
	[(a :. (b :. (Maybe c :. Maybe d)))] ->
	[(a, b, [(c, d)])]
ojoin2of4 xs = map (regroup3 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple2of4 xs

ojoin3of4 :: (Eq a, Eq b, Eq c, Eq d) =>
	[(a :. (b :. (c :. Maybe d)))] ->
	[(a, b, c, [d])]
ojoin3of4 xs = map (regroup4 . second catMaybes) $ groupJoin $ map tuple3of4 xs

---------------------------------------------------------------------- | Groups of 5

ojoin1of5 :: (Eq a, Eq b, Eq c, Eq d, Eq e) =>
	[(a :. (Maybe b :. (Maybe c :. (Maybe d :. Maybe e))))] ->
	[(a, [(b, c, d, e)])]
ojoin1of5 xs = map (second (mapMaybe sequenceT)) $ groupJoin $ map tuple1of5 xs

ojoin2of5 :: (Eq a, Eq b, Eq c, Eq d, Eq e) =>
	[(a :. (b :. (Maybe c :. (Maybe d :. Maybe e))))] ->
	[(a, b, [(c, d, e)])]
ojoin2of5 xs = map (regroup3 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple2of5 xs

ojoin3of5 :: (Eq a, Eq b, Eq c, Eq d, Eq e) =>
	[(a :. (b :. (c :. (Maybe d :. Maybe e))))] ->
	[(a, b, c, [(d, e)])]
ojoin3of5 xs = map (regroup4 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple3of5 xs

ojoin4of5 :: (Eq a, Eq b, Eq c, Eq d, Eq e) =>
	[(a :. (b :. (c :. (d :. Maybe e))))] ->
	[(a, b, c, d, [e])]
ojoin4of5 xs = map (regroup5 . second catMaybes) $ groupJoin $ map tuple4of5 xs

---------------------------------------------------------------------- | Groups of 6

ojoin1of6 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) =>
	[(a :. (Maybe b :. (Maybe c :. (Maybe d :. (Maybe e :. Maybe f)))))] ->
	[(a, [(b, c, d, e, f)])]
ojoin1of6 xs = map (second (mapMaybe sequenceT)) $ groupJoin $ map tuple1of6 xs

ojoin2of6 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) =>
	[(a :. (b :. (Maybe c :. (Maybe d :. (Maybe e :. Maybe f)))))] ->
	[(a, b, [(c, d, e, f)])]
ojoin2of6 xs = map (regroup3 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple2of6 xs

ojoin3of6 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) =>
	[(a :. (b :. (c :. (Maybe d :. (Maybe e :. Maybe f)))))] ->
	[(a, b, c, [(d, e, f)])]
ojoin3of6 xs = map (regroup4 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple3of6 xs

ojoin4of6 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) =>
	[(a :. (b :. (c :. (d :. (Maybe e :. Maybe f)))))] ->
	[(a, b, c, d, [(e, f)])]
ojoin4of6 xs = map (regroup5 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple4of6 xs

ojoin5of6 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) =>
	[(a :. (b :. (c :. (d :. (e :. Maybe f)))))] ->
	[(a, b, c, d, e, [f])]
ojoin5of6 xs = map (regroup6 . second catMaybes) $ groupJoin $ map tuple5of6 xs

---------------------------------------------------------------------- | Groups of 7

ojoin1of7 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) =>
	[(a :. (Maybe b :. (Maybe c :. (Maybe d :. (Maybe e :. (Maybe f :. Maybe g))))))] ->
	[(a, [(b, c, d, e, f, g)])]
ojoin1of7 xs = map (second (mapMaybe sequenceT)) $ groupJoin $ map tuple1of7 xs

ojoin2of7 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) =>
	[(a :. (b :. (Maybe c :. (Maybe d :. (Maybe e :. (Maybe f :. Maybe g))))))] ->
	[(a, b, [(c, d, e, f, g)])]
ojoin2of7 xs = map (regroup3 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple2of7 xs

ojoin3of7 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) =>
	[(a :. (b :. (c :. (Maybe d :. (Maybe e :. (Maybe f :. Maybe g))))))] ->
	[(a, b, c, [(d, e, f, g)])]
ojoin3of7 xs = map (regroup4 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple3of7 xs

ojoin4of7 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) =>
	[(a :. (b :. (c :. (d :. (Maybe e :. (Maybe f :. Maybe g))))))] ->
	[(a, b, c, d, [(e, f, g)])]
ojoin4of7 xs = map (regroup5 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple4of7 xs

ojoin5of7 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) =>
	[(a :. (b :. (c :. (d :. (e :. (Maybe f :. Maybe g))))))] ->
	[(a, b, c, d, e, [(f, g)])]
ojoin5of7 xs = map (regroup6 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple5of7 xs

ojoin6of7 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. Maybe g))))))] ->
	[(a, b, c, d, e, f, [g])]
ojoin6of7 xs = map (regroup7 . second catMaybes) $ groupJoin $ map tuple6of7 xs

---------------------------------------------------------------------- | Groups of 8

ojoin1of8 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) =>
	[(a :. (Maybe b :. (Maybe c :. (Maybe d :. (Maybe e :. (Maybe f :. (Maybe g :. Maybe h)))))))] ->
	[(a, [(b, c, d, e, f, g, h)])]
ojoin1of8 xs = map (second (mapMaybe sequenceT)) $ groupJoin $ map tuple1of8 xs

ojoin2of8 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) =>
	[(a :. (b :. (Maybe c :. (Maybe d :. (Maybe e :. (Maybe f :. (Maybe g :. Maybe h)))))))] ->
	[(a, b, [(c, d, e, f, g, h)])]
ojoin2of8 xs = map (regroup3 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple2of8 xs

ojoin3of8 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) =>
	[(a :. (b :. (c :. (Maybe d :. (Maybe e :. (Maybe f :. (Maybe g :. Maybe h)))))))] ->
	[(a, b, c, [(d, e, f, g, h)])]
ojoin3of8 xs = map (regroup4 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple3of8 xs

ojoin4of8 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) =>
	[(a :. (b :. (c :. (d :. (Maybe e :. (Maybe f :. (Maybe g :. Maybe h)))))))] ->
	[(a, b, c, d, [(e, f, g, h)])]
ojoin4of8 xs = map (regroup5 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple4of8 xs

ojoin5of8 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) =>
	[(a :. (b :. (c :. (d :. (e :. (Maybe f :. (Maybe g :. Maybe h)))))))] ->
	[(a, b, c, d, e, [(f, g, h)])]
ojoin5of8 xs = map (regroup6 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple5of8 xs

ojoin6of8 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (Maybe g :. Maybe h)))))))] ->
	[(a, b, c, d, e, f, [(g, h)])]
ojoin6of8 xs = map (regroup7 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple6of8 xs

ojoin7of8 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. Maybe h)))))))] ->
	[(a, b, c, d, e, f, g, [h])]
ojoin7of8 xs = map (regroup8 . second catMaybes) $ groupJoin $ map tuple7of8 xs

---------------------------------------------------------------------- | Groups of 9

ojoin1of9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) =>
	[(a :. (Maybe b :. (Maybe c :. (Maybe d :. (Maybe e :. (Maybe f :. (Maybe g :. (Maybe h :. Maybe i))))))))] ->
	[(a, [(b, c, d, e, f, g, h, i)])]
ojoin1of9 xs = map (second (mapMaybe sequenceT)) $ groupJoin $ map tuple1of9 xs

ojoin2of9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) =>
	[(a :. (b :. (Maybe c :. (Maybe d :. (Maybe e :. (Maybe f :. (Maybe g :. (Maybe h :. Maybe i))))))))] ->
	[(a, b, [(c, d, e, f, g, h, i)])]
ojoin2of9 xs = map (regroup3 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple2of9 xs

ojoin3of9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) =>
	[(a :. (b :. (c :. (Maybe d :. (Maybe e :. (Maybe f :. (Maybe g :. (Maybe h :. Maybe i))))))))] ->
	[(a, b, c, [(d, e, f, g, h, i)])]
ojoin3of9 xs = map (regroup4 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple3of9 xs

ojoin4of9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) =>
	[(a :. (b :. (c :. (d :. (Maybe e :. (Maybe f :. (Maybe g :. (Maybe h :. Maybe i))))))))] ->
	[(a, b, c, d, [(e, f, g, h, i)])]
ojoin4of9 xs = map (regroup5 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple4of9 xs

ojoin5of9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) =>
	[(a :. (b :. (c :. (d :. (e :. (Maybe f :. (Maybe g :. (Maybe h :. Maybe i))))))))] ->
	[(a, b, c, d, e, [(f, g, h, i)])]
ojoin5of9 xs = map (regroup6 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple5of9 xs

ojoin6of9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (Maybe g :. (Maybe h :. Maybe i))))))))] ->
	[(a, b, c, d, e, f, [(g, h, i)])]
ojoin6of9 xs = map (regroup7 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple6of9 xs

ojoin7of9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (Maybe h :. Maybe i))))))))] ->
	[(a, b, c, d, e, f, g, [(h, i)])]
ojoin7of9 xs = map (regroup8 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple7of9 xs

ojoin8of9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. Maybe i))))))))] ->
	[((a, b, c, d, e, f, g, h), [i])]
ojoin8of9 xs = map (second catMaybes) $ groupJoin $ map tuple8of9 xs

---------------------------------------------------------------------- | Groups of 10

ojoin1of10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) =>
	[(a :. (Maybe b :. (Maybe c :. (Maybe d :. (Maybe e :. (Maybe f :. (Maybe g :. (Maybe h :. (Maybe i :. Maybe j)))))))))] ->
	[(a, [(b, c, d, e, f, g, h, i, j)])]
ojoin1of10 xs = map (second (mapMaybe sequenceT)) $ groupJoin $ map tuple1of10 xs

ojoin2of10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) =>
	[(a :. (b :. (Maybe c :. (Maybe d :. (Maybe e :. (Maybe f :. (Maybe g :. (Maybe h :. (Maybe i :. Maybe j)))))))))] ->
	[(a, b, [(c, d, e, f, g, h, i, j)])]
ojoin2of10 xs = map (regroup3 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple2of10 xs

ojoin3of10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) =>
	[(a :. (b :. (c :. (Maybe d :. (Maybe e :. (Maybe f :. (Maybe g :. (Maybe h :. (Maybe i :. Maybe j)))))))))] ->
	[(a, b, c, [(d, e, f, g, h, i, j)])]
ojoin3of10 xs = map (regroup4 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple3of10 xs

ojoin4of10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) =>
	[(a :. (b :. (c :. (d :. (Maybe e :. (Maybe f :. (Maybe g :. (Maybe h :. (Maybe i :. Maybe j)))))))))] ->
	[(a, b, c, d, [(e, f, g, h, i, j)])]
ojoin4of10 xs = map (regroup5 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple4of10 xs

ojoin5of10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) =>
	[(a :. (b :. (c :. (d :. (e :. (Maybe f :. (Maybe g :. (Maybe h :. (Maybe i :. Maybe j)))))))))] ->
	[(a, b, c, d, e, [(f, g, h, i, j)])]
ojoin5of10 xs = map (regroup6 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple5of10 xs

ojoin6of10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (Maybe g :. (Maybe h :. (Maybe i :. Maybe j)))))))))] ->
	[(a, b, c, d, e, f, [(g, h, i, j)])]
ojoin6of10 xs = map (regroup7 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple6of10 xs

ojoin7of10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (Maybe h :. (Maybe i :. Maybe j)))))))))] ->
	[(a, b, c, d, e, f, g, [(h, i, j)])]
ojoin7of10 xs = map (regroup8 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple7of10 xs

ojoin8of10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (Maybe i :. Maybe j)))))))))] ->
	[(a, b, c, d, e, f, g, h, [(i, j)])]
ojoin8of10 xs = map (regroup9 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple8of10 xs

ojoin9of10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. Maybe j)))))))))] ->
	[(a, b, c, d, e, f, g, h, i, [j])]
ojoin9of10 xs = map (regroup10 . second catMaybes) $ groupJoin $ map tuple9of10 xs

---------------------------------------------------------------------- | Groups of 11

ojoin1of11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
	[(a :. (Maybe b :. (Maybe c :. (Maybe d :. (Maybe e :. (Maybe f :. (Maybe g :. (Maybe h :. (Maybe i :. (Maybe j :. Maybe k))))))))))] ->
	[(a, [(b, c, d, e, f, g, h, i, j, k)])]
ojoin1of11 xs = map (second (mapMaybe sequenceT)) $ groupJoin $ map tuple1of11 xs

ojoin2of11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
	[(a :. (b :. (Maybe c :. (Maybe d :. (Maybe e :. (Maybe f :. (Maybe g :. (Maybe h :. (Maybe i :. (Maybe j :. Maybe k))))))))))] ->
	[(a, b, [(c, d, e, f, g, h, i, j, k)])]
ojoin2of11 xs = map (regroup3 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple2of11 xs

ojoin3of11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
	[(a :. (b :. (c :. (Maybe d :. (Maybe e :. (Maybe f :. (Maybe g :. (Maybe h :. (Maybe i :. (Maybe j :. Maybe k))))))))))] ->
	[(a, b, c, [(d, e, f, g, h, i, j, k)])]
ojoin3of11 xs = map (regroup4 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple3of11 xs

ojoin4of11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
	[(a :. (b :. (c :. (d :. (Maybe e :. (Maybe f :. (Maybe g :. (Maybe h :. (Maybe i :. (Maybe j :. Maybe k))))))))))] ->
	[(a, b, c, d, [(e, f, g, h, i, j, k)])]
ojoin4of11 xs = map (regroup5 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple4of11 xs

ojoin5of11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
	[(a :. (b :. (c :. (d :. (e :. (Maybe f :. (Maybe g :. (Maybe h :. (Maybe i :. (Maybe j :. Maybe k))))))))))] ->
	[(a, b, c, d, e, [(f, g, h, i, j, k)])]
ojoin5of11 xs = map (regroup6 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple5of11 xs

ojoin6of11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (Maybe g :. (Maybe h :. (Maybe i :. (Maybe j :. Maybe k))))))))))] ->
	[(a, b, c, d, e, f, [(g, h, i, j, k)])]
ojoin6of11 xs = map (regroup7 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple6of11 xs

ojoin7of11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (Maybe h :. (Maybe i :. (Maybe j :. Maybe k))))))))))] ->
	[(a, b, c, d, e, f, g, [(h, i, j, k)])]
ojoin7of11 xs = map (regroup8 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple7of11 xs

ojoin8of11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (Maybe i :. (Maybe j :. Maybe k))))))))))] ->
	[(a, b, c, d, e, f, g, h, [(i, j, k)])]
ojoin8of11 xs = map (regroup9 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple8of11 xs

ojoin9of11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (Maybe j :. Maybe k))))))))))] ->
	[(a, b, c, d, e, f, g, h, i, [(j, k)])]
ojoin9of11 xs = map (regroup10 . second (mapMaybe sequenceT)) $ groupJoin $ map tuple9of11 xs

ojoin10of11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) =>
	[(a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. Maybe k))))))))))] ->
	[(a, b, c, d, e, f, g, h, i, j, [k])]
ojoin10of11 xs = map (regroup11 . second catMaybes) $ groupJoin $ map tuple10of11 xs
