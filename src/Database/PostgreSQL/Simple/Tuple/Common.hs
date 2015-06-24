{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances, TypeOperators #-}

module Database.PostgreSQL.Simple.Tuple.Common
	( groupJoin
	, tuple2
	, tuple3
	, tuple4
	, tuple5
	, tuple6
	, tuple7
	, tuple8
	, tuple9
	, tuple10
	, tuple11

	, tuple1of3
	, tuple2of3
	, tuple1of4
	, tuple2of4
	, tuple3of4
	, tuple1of5
	, tuple2of5
	, tuple3of5
	, tuple4of5
	, tuple1of6
	, tuple2of6
	, tuple3of6
	, tuple4of6
	, tuple5of6
	, tuple1of7
	, tuple2of7
	, tuple3of7
	, tuple4of7
	, tuple5of7
	, tuple6of7
	, tuple1of8
	, tuple2of8
	, tuple3of8
	, tuple4of8
	, tuple5of8
	, tuple6of8
	, tuple7of8
	, tuple1of9
	, tuple2of9
	, tuple3of9
	, tuple4of9
	, tuple5of9
	, tuple6of9
	, tuple7of9
	, tuple8of9
	, tuple1of10
	, tuple2of10
	, tuple3of10
	, tuple4of10
	, tuple5of10
	, tuple6of10
	, tuple7of10
	, tuple8of10
	, tuple9of10
	, tuple1of11
	, tuple2of11
	, tuple3of11
	, tuple4of11
	, tuple5of11
	, tuple6of11
	, tuple7of11
	, tuple8of11
	, tuple9of11
	, tuple10of11
	)
where

import Control.Arrow (second)
import Data.Maybe (catMaybes, mapMaybe, isJust)
import Data.List (groupBy)
import Data.Function (on)
import Database.PostgreSQL.Simple ((:.)(..))

groupJoin :: Eq a => [(a, b)] -> [(a, [b])]
groupJoin xs = map reduce $ groupBy ((==) `on` fst) xs where
	reduce xs' = (fst $ head xs', map snd xs')

{----------------------------------------------------------------------------------------------------{
                                                                       | Simple Tuples
}----------------------------------------------------------------------------------------------------}

tuple2 :: (a :. b) -> (a, b)
tuple2 (a:.b) = (a,b)

tuple3 :: (a :. (b :. c)) -> (a, b, c)
tuple3 (a:.b:.c) = (a,b,c)

tuple4 :: (a :. (b :. (c :. d))) -> (a, b, c, d)
tuple4 (a:.b:.c:.d) = (a,b,c,d)

tuple5 :: (a :. (b :. (c :. (d :. e)))) -> (a, b, c, d, e)
tuple5 (a:.b:.c:.d:.e) = (a,b,c,d,e)

tuple6 :: (a :. (b :. (c :. (d :. (e :. f))))) -> (a, b, c, d, e, f)
tuple6 (a:.b:.c:.d:.e:.f) = (a,b,c,d,e,f)

tuple7 :: (a :. (b :. (c :. (d :. (e :. (f :. g)))))) -> (a, b, c, d, e, f, g)
tuple7 (a:.b:.c:.d:.e:.f:.g) = (a,b,c,d,e,f,g)

tuple8 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. h))))))) -> (a, b, c, d, e, f, g, h)
tuple8 (a:.b:.c:.d:.e:.f:.g:.h) = (a,b,c,d,e,f,g,h)

tuple9 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. i)))))))) -> (a, b, c, d, e, f, g, h, i)
tuple9 (a:.b:.c:.d:.e:.f:.g:.h:.i) = (a,b,c,d,e,f,g,h,i)

tuple10 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. j))))))))) -> (a, b, c, d, e, f, g, h, i, j)
tuple10 (a:.b:.c:.d:.e:.f:.g:.h:.i:.j) = (a,b,c,d,e,f,g,h,i,j)

tuple11 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. k)))))))))) -> (a, b, c, d, e, f, g, h, i, j, k)
tuple11 (a:.b:.c:.d:.e:.f:.g:.h:.i:.j:.k) = (a,b,c,d,e,f,g,h,i,j,k)

{----------------------------------------------------------------------------------------------------{
                                                                       | Grouped Tuples
}----------------------------------------------------------------------------------------------------}

---------------------------------------------------------------------- | Groups of 3

tuple1of3 :: (a :. (b :. c)) -> (a, (b, c))
tuple1of3 (a:.b:.c) = (a,(b,c))

tuple2of3 :: (a :. (b :. c)) -> ((a, b), c)
tuple2of3 (a:.b:.c) = ((a,b),c)

---------------------------------------------------------------------- | Groups of 4

tuple1of4 :: (a :. (b :. (c :. d))) -> (a, (b, c, d))
tuple1of4 (a:.b:.c:.d) = (a,(b,c,d))

tuple2of4 :: (a :. (b :. (c :. d))) -> ((a, b), (c, d))
tuple2of4 (a:.b:.c:.d) = ((a,b),(c,d))

tuple3of4 :: (a :. (b :. (c :. d))) -> ((a, b, c), d)
tuple3of4 (a:.b:.c:.d) = ((a,b,c),d)

---------------------------------------------------------------------- | Groups of 5

tuple1of5 :: (a :. (b :. (c :. (d :. e)))) -> (a, (b, c, d, e))
tuple1of5 (a:.b:.c:.d:.e) = (a,(b,c,d,e))

tuple2of5 :: (a :. (b :. (c :. (d :. e)))) -> ((a, b), (c, d, e))
tuple2of5 (a:.b:.c:.d:.e) = ((a,b),(c,d,e))

tuple3of5 :: (a :. (b :. (c :. (d :. e)))) -> ((a, b, c), (d, e))
tuple3of5 (a:.b:.c:.d:.e) = ((a,b,c),(d,e))

tuple4of5 :: (a :. (b :. (c :. (d :. e)))) -> ((a, b, c, d), e)
tuple4of5 (a:.b:.c:.d:.e) = ((a,b,c,d),e)

---------------------------------------------------------------------- | Groups of 6

tuple1of6 :: (a :. (b :. (c :. (d :. (e :. f))))) -> (a, (b, c, d, e, f))
tuple1of6 (a:.b:.c:.d:.e:.f) = (a,(b,c,d,e,f))

tuple2of6 :: (a :. (b :. (c :. (d :. (e :. f))))) -> ((a, b), (c, d, e, f))
tuple2of6 (a:.b:.c:.d:.e:.f) = ((a,b),(c,d,e,f))

tuple3of6 :: (a :. (b :. (c :. (d :. (e :. f))))) -> ((a, b, c), (d, e, f))
tuple3of6 (a:.b:.c:.d:.e:.f) = ((a,b,c),(d,e,f))

tuple4of6 :: (a :. (b :. (c :. (d :. (e :. f))))) -> ((a, b, c, d), (e, f))
tuple4of6 (a:.b:.c:.d:.e:.f) = ((a,b,c,d),(e,f))

tuple5of6 :: (a :. (b :. (c :. (d :. (e :. f))))) -> ((a, b, c, d, e), f)
tuple5of6 (a:.b:.c:.d:.e:.f) = ((a,b,c,d,e),f)

---------------------------------------------------------------------- | Groups of 7

tuple1of7 :: (a :. (b :. (c :. (d :. (e :. (f :. g)))))) -> (a, (b, c, d, e, f, g))
tuple1of7 (a:.b:.c:.d:.e:.f:.g) = (a,(b,c,d,e,f,g))

tuple2of7 :: (a :. (b :. (c :. (d :. (e :. (f :. g)))))) -> ((a, b), (c, d, e, f, g))
tuple2of7 (a:.b:.c:.d:.e:.f:.g) = ((a,b),(c,d,e,f,g))

tuple3of7 :: (a :. (b :. (c :. (d :. (e :. (f :. g)))))) -> ((a, b, c), (d, e, f, g))
tuple3of7 (a:.b:.c:.d:.e:.f:.g) = ((a,b,c),(d,e,f,g))

tuple4of7 :: (a :. (b :. (c :. (d :. (e :. (f :. g)))))) -> ((a, b, c, d), (e, f, g))
tuple4of7 (a:.b:.c:.d:.e:.f:.g) = ((a,b,c,d),(e,f,g))

tuple5of7 :: (a :. (b :. (c :. (d :. (e :. (f :. g)))))) -> ((a, b, c, d, e), (f, g))
tuple5of7 (a:.b:.c:.d:.e:.f:.g) = ((a,b,c,d,e),(f,g))

tuple6of7 :: (a :. (b :. (c :. (d :. (e :. (f :. g)))))) -> ((a, b, c, d, e, f), g)
tuple6of7 (a:.b:.c:.d:.e:.f:.g) = ((a,b,c,d,e,f),g)

---------------------------------------------------------------------- | Groups of 8

tuple1of8 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. h))))))) -> (a, (b, c, d, e, f, g, h))
tuple1of8 (a:.b:.c:.d:.e:.f:.g:.h) = (a,(b,c,d,e,f,g,h))

tuple2of8 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. h))))))) -> ((a, b), (c, d, e, f, g, h))
tuple2of8 (a:.b:.c:.d:.e:.f:.g:.h) = ((a,b),(c,d,e,f,g,h))

tuple3of8 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. h))))))) -> ((a, b, c), (d, e, f, g, h))
tuple3of8 (a:.b:.c:.d:.e:.f:.g:.h) = ((a,b,c),(d,e,f,g,h))

tuple4of8 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. h))))))) -> ((a, b, c, d), (e, f, g, h))
tuple4of8 (a:.b:.c:.d:.e:.f:.g:.h) = ((a,b,c,d),(e,f,g,h))

tuple5of8 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. h))))))) -> ((a, b, c, d, e), (f, g, h))
tuple5of8 (a:.b:.c:.d:.e:.f:.g:.h) = ((a,b,c,d,e),(f,g,h))

tuple6of8 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. h))))))) -> ((a, b, c, d, e, f), (g, h))
tuple6of8 (a:.b:.c:.d:.e:.f:.g:.h) = ((a,b,c,d,e,f),(g,h))

tuple7of8 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. h))))))) -> ((a, b, c, d, e, f, g), h)
tuple7of8 (a:.b:.c:.d:.e:.f:.g:.h) = ((a,b,c,d,e,f,g),h)

---------------------------------------------------------------------- | Groups of 9

tuple1of9 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. i)))))))) -> (a, (b, c, d, e, f, g, h, i))
tuple1of9 (a:.b:.c:.d:.e:.f:.g:.h:.i) = (a,(b,c,d,e,f,g,h,i))

tuple2of9 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. i)))))))) -> ((a, b), (c, d, e, f, g, h, i))
tuple2of9 (a:.b:.c:.d:.e:.f:.g:.h:.i) = ((a,b),(c,d,e,f,g,h,i))

tuple3of9 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. i)))))))) -> ((a, b, c), (d, e, f, g, h, i))
tuple3of9 (a:.b:.c:.d:.e:.f:.g:.h:.i) = ((a,b,c),(d,e,f,g,h,i))

tuple4of9 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. i)))))))) -> ((a, b, c, d), (e, f, g, h, i))
tuple4of9 (a:.b:.c:.d:.e:.f:.g:.h:.i) = ((a,b,c,d),(e,f,g,h,i))

tuple5of9 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. i)))))))) -> ((a, b, c, d, e), (f, g, h, i))
tuple5of9 (a:.b:.c:.d:.e:.f:.g:.h:.i) = ((a,b,c,d,e),(f,g,h,i))

tuple6of9 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. i)))))))) -> ((a, b, c, d, e, f), (g, h, i))
tuple6of9 (a:.b:.c:.d:.e:.f:.g:.h:.i) = ((a,b,c,d,e,f),(g,h,i))

tuple7of9 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. i)))))))) -> ((a, b, c, d, e, f, g), (h, i))
tuple7of9 (a:.b:.c:.d:.e:.f:.g:.h:.i) = ((a,b,c,d,e,f,g),(h,i))

tuple8of9 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. i)))))))) -> ((a, b, c, d, e, f, g, h), i)
tuple8of9 (a:.b:.c:.d:.e:.f:.g:.h:.i) = ((a,b,c,d,e,f,g,h),i)

---------------------------------------------------------------------- | Groups of 10

tuple1of10 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. j))))))))) -> (a, (b, c, d, e, f, g, h, i, j))
tuple1of10 (a:.b:.c:.d:.e:.f:.g:.h:.i:.j) = (a,(b,c,d,e,f,g,h,i,j))

tuple2of10 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. j))))))))) -> ((a, b), (c, d, e, f, g, h, i, j))
tuple2of10 (a:.b:.c:.d:.e:.f:.g:.h:.i:.j) = ((a,b),(c,d,e,f,g,h,i,j))

tuple3of10 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. j))))))))) -> ((a, b, c), (d, e, f, g, h, i, j))
tuple3of10 (a:.b:.c:.d:.e:.f:.g:.h:.i:.j) = ((a,b,c),(d,e,f,g,h,i,j))

tuple4of10 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. j))))))))) -> ((a, b, c, d), (e, f, g, h, i, j))
tuple4of10 (a:.b:.c:.d:.e:.f:.g:.h:.i:.j) = ((a,b,c,d),(e,f,g,h,i,j))

tuple5of10 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. j))))))))) -> ((a, b, c, d, e), (f, g, h, i, j))
tuple5of10 (a:.b:.c:.d:.e:.f:.g:.h:.i:.j) = ((a,b,c,d,e),(f,g,h,i,j))

tuple6of10 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. j))))))))) -> ((a, b, c, d, e, f), (g, h, i, j))
tuple6of10 (a:.b:.c:.d:.e:.f:.g:.h:.i:.j) = ((a,b,c,d,e,f),(g,h,i,j))

tuple7of10 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. j))))))))) -> ((a, b, c, d, e, f, g), (h, i, j))
tuple7of10 (a:.b:.c:.d:.e:.f:.g:.h:.i:.j) = ((a,b,c,d,e,f,g),(h,i,j))

tuple8of10 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. j))))))))) -> ((a, b, c, d, e, f, g, h), (i, j))
tuple8of10 (a:.b:.c:.d:.e:.f:.g:.h:.i:.j) = ((a,b,c,d,e,f,g,h),(i,j))

tuple9of10 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. j))))))))) -> ((a, b, c, d, e, f, g, h, i), j)
tuple9of10 (a:.b:.c:.d:.e:.f:.g:.h:.i:.j) = ((a,b,c,d,e,f,g,h,i),j)

---------------------------------------------------------------------- | Groups of 11

tuple1of11 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. k)))))))))) -> (a, (b, c, d, e, f, g, h, i, j, k))
tuple1of11 (a:.b:.c:.d:.e:.f:.g:.h:.i:.j:.k) = (a,(b,c,d,e,f,g,h,i,j,k))

tuple2of11 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. k)))))))))) -> ((a, b), (c, d, e, f, g, h, i, j, k))
tuple2of11 (a:.b:.c:.d:.e:.f:.g:.h:.i:.j:.k) = ((a,b),(c,d,e,f,g,h,i,j,k))

tuple3of11 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. k)))))))))) -> ((a, b, c), (d, e, f, g, h, i, j, k))
tuple3of11 (a:.b:.c:.d:.e:.f:.g:.h:.i:.j:.k) = ((a,b,c),(d,e,f,g,h,i,j,k))

tuple4of11 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. k)))))))))) -> ((a, b, c, d), (e, f, g, h, i, j, k))
tuple4of11 (a:.b:.c:.d:.e:.f:.g:.h:.i:.j:.k) = ((a,b,c,d),(e,f,g,h,i,j,k))

tuple5of11 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. k)))))))))) -> ((a, b, c, d, e), (f, g, h, i, j, k))
tuple5of11 (a:.b:.c:.d:.e:.f:.g:.h:.i:.j:.k) = ((a,b,c,d,e),(f,g,h,i,j,k))

tuple6of11 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. k)))))))))) -> ((a, b, c, d, e, f), (g, h, i, j, k))
tuple6of11 (a:.b:.c:.d:.e:.f:.g:.h:.i:.j:.k) = ((a,b,c,d,e,f),(g,h,i,j,k))

tuple7of11 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. k)))))))))) -> ((a, b, c, d, e, f, g), (h, i, j, k))
tuple7of11 (a:.b:.c:.d:.e:.f:.g:.h:.i:.j:.k) = ((a,b,c,d,e,f,g),(h,i,j,k))

tuple8of11 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. k)))))))))) -> ((a, b, c, d, e, f, g, h), (i, j, k))
tuple8of11 (a:.b:.c:.d:.e:.f:.g:.h:.i:.j:.k) = ((a,b,c,d,e,f,g,h),(i,j,k))

tuple9of11 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. k)))))))))) -> ((a, b, c, d, e, f, g, h, i), (j, k))
tuple9of11 (a:.b:.c:.d:.e:.f:.g:.h:.i:.j:.k) = ((a,b,c,d,e,f,g,h,i),(j,k))

tuple10of11 :: (a :. (b :. (c :. (d :. (e :. (f :. (g :. (h :. (i :. (j :. k)))))))))) -> ((a, b, c, d, e, f, g, h, i, j), k)
tuple10of11 (a:.b:.c:.d:.e:.f:.g:.h:.i:.j:.k) = ((a,b,c,d,e,f,g,h,i,j),k)
