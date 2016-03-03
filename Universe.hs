{-
    This code taken from Habrahabr:
    http://habrahabr.ru/post/225473/
-}
module Universe where

import Control.Comonad
import qualified Data.Vector as V

type Current = Int
data Universe a = Universe (V.Vector a) Int
newtype Universe2 a = Universe2 { getUniverse2 :: Universe (Universe a) } -- TODO: is this type necessary? For comonad implementaion?

left :: Universe a -> Universe a
left (Universe v 0)   = Universe v (V.length v - 1)
left (Universe v cur) = Universe v (cur - 1)

right :: Universe a -> Universe a
right (Universe v cur) | cur == (V.length v - 1) = Universe v 0
                       | otherwise               = Universe v (cur + 1)

left2 = left . left
left3 = left . left . left

right2 = right . right
right3 = right . right . right

makeUniverse fl fr x@(Universe v cur) = newUniverse
  where
    s   = V.length v
    lvs = if cur == 0     then [] else take cur           . tail $ iterate fl x
    rvs = if cur == s - 1 then [] else take (s - cur - 1) . tail $ iterate fr x
    vs  = lvs ++ [x] ++ rvs
    newUniverse = Universe (V.fromList vs) cur

instance Functor Universe where
    fmap f (Universe v cur) = Universe (V.map f v) cur

instance Comonad Universe where
    duplicate = makeUniverse left right
    extract (Universe v cur) = V.unsafeIndex v cur

takeRange :: (Int, Int) -> Universe a -> [a]
takeRange (a, b) (Universe v _) | a <  0 = V.toList $ V.slice (1 - a) (b - a + 1) v
takeRange (a, b) (Universe v _) | a >= 0 = V.toList $ V.slice (a - 1) (b - a + 1) v

instance Functor Universe2 where
    fmap f = Universe2 . (fmap . fmap) f . getUniverse2

instance Comonad Universe2 where
    extract = extract . extract . getUniverse2
    duplicate = fmap Universe2 . Universe2 . shifted . shifted . getUniverse2
        where shifted :: Universe (Universe a) -> Universe (Universe (Universe a))
              shifted = makeUniverse (fmap left) (fmap right)

stepWith :: Universe2 a -> (Universe2 a -> a) -> Universe2 a
stepWith = (=>>)

fromListU :: Int -> a -> [a] -> Universe a
fromListU _ _ [] = error "Empty list."
fromListU 0 _ _ = error "Invalid size."
fromListU s zeroC xs | length xs > s = error "Invalid bounds."
                     | otherwise =
    let unfilled     = s - (length xs)
        unfR         = unfilled `div` 2
        unfL         = unfilled - unfR
        rsL          = replicate unfL zeroC
        rsR          = replicate unfR zeroC
        xs'          = rsL ++ xs ++ rsR
    in Universe (V.fromList xs') 0

fromList2 :: Int -> a -> [[a]] -> Universe2 a
fromList2 size zeroC xss = Universe2 vss
  where
    zeroU = fromListU size zeroC . replicate size $ zeroC
    vs    = map (fromListU size zeroC) xss
    vss   = fromListU size zeroU vs

toList2 :: Universe2 a -> [[a]]
toList2 u = vs
  where
    (Universe vu' _) = getUniverse2 u
    getCells (Universe v _) = V.toList v
    vs = map getCells (V.toList vu')
       



