module Database.PostgreSQL.Simple.Tuple.Maybe {-# DEPRECATED "Use Data.Tuple.Sequence instead" #-} where

import Data.Maybe (catMaybes, mapMaybe, isJust)

cat2Maybes :: (Maybe a, Maybe b) -> Maybe (Maybe a, Maybe b)
cat2Maybes (a, b) = if isJust a && isJust b then Just (a, b) else Nothing

cat3Maybes :: (Maybe a, Maybe b, Maybe c) -> Maybe (Maybe a, Maybe b, Maybe c)
cat3Maybes (a, b, c) = if isJust a && isJust b && isJust c then Just (a, b, c) else Nothing

cat4Maybes :: (Maybe a, Maybe b, Maybe c, Maybe d) -> Maybe (Maybe a, Maybe b, Maybe c, Maybe d)
cat4Maybes (a, b, c, d) = if isJust a && isJust b && isJust c && isJust d then Just (a, b, c, d) else Nothing

cat5Maybes :: (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e) -> Maybe (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e)
cat5Maybes (a, b, c, d, e) = if isJust a && isJust b && isJust c && isJust d && isJust e then Just (a, b, c, d, e) else Nothing

cat6Maybes :: (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f) -> Maybe (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f)
cat6Maybes (a, b, c, d, e, f) = if isJust a && isJust b && isJust c && isJust d && isJust e && isJust f then Just (a, b, c, d, e, f) else Nothing

cat7Maybes :: (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f, Maybe g) -> Maybe (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f, Maybe g)
cat7Maybes (a, b, c, d, e, f, h) = if isJust a && isJust b && isJust c && isJust d && isJust e && isJust f && isJust h then Just (a, b, c, d, e, f, h) else Nothing

cat8Maybes :: (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f, Maybe g, Maybe h) -> Maybe (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f, Maybe g, Maybe h)
cat8Maybes (a, b, c, d, e, f, g, h) = if isJust a && isJust b && isJust c && isJust d && isJust e && isJust f && isJust g && isJust h then Just (a, b, c, d, e, f, g, h) else Nothing

cat9Maybes :: (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f, Maybe g, Maybe h, Maybe i) -> Maybe (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f, Maybe g, Maybe h, Maybe i)
cat9Maybes (a, b, c, d, e, f, g, h, i) = if isJust a && isJust b && isJust c && isJust d && isJust e && isJust f&& isJust g && isJust h && isJust i then Just (a, b, c, d, e, f, g, h, i) else Nothing

cat10Maybes :: (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f, Maybe g, Maybe h, Maybe i, Maybe j) -> Maybe (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f, Maybe g, Maybe h, Maybe i, Maybe j)
cat10Maybes (a, b, c, d, e, f, g, h, i, j) = if isJust a && isJust b && isJust c && isJust d && isJust e && isJust f && isJust g && isJust h && isJust i && isJust j then Just (a, b, c, d, e, f, g, h, i, j) else Nothing

--cat11Maybes :: (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f, Maybe g, Maybe h, Maybe i, Maybe j, Maybe k) -> Maybe (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f, Maybe g, Maybe h, Maybe i, Maybe j, Maybe k)
--cat11Maybes (a, b, c, d, e, f, g, h, i, j, k) = if isJust a && isJust b && isJust c && isJust d && isJust e && isJust f && isJust g && isJust h && isJust i && isJust j && isJust k then Just (a, b, c, d, e, f, g, h, i, j, k) else Nothing

