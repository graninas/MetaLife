module UniverseNeighbours where

import Universe

import Control.Applicative
import Control.Comonad

nearest3 :: Universe a -> [a]
nearest3 u = fmap extract [left u, u, right u]

nearest5 :: Universe a -> [a]
nearest5 u = fmap extract [left2 u, right2 u] ++ nearest3 u

nearest7 :: Universe a -> [a]
nearest7 u = fmap extract [left3 u, right3 u] ++ nearest5 u

neighbours'' :: (Universe2 a) -> [a]
neighbours'' u =
    [ nearest7 . extract . left3
    , pure     . extract . left3  . extract . left
    , pure     . extract . right3 . extract . left
    , pure     . extract . left3  . extract . left2
    , pure     . extract . right3 . extract . left2
    , pure     . extract . left3  . extract
    , pure     . extract . right3 . extract
    , pure     . extract . left3  . extract . right
    , pure     . extract . right3 . extract . right
    , pure     . extract . left3  . extract . right2
    , pure     . extract . right3 . extract . right2
    , nearest7 . extract . right3
    ] >>= ($ getUniverse2 u)

neighbours' :: (Universe2 a) -> [a]
neighbours' u =
    [ nearest5 . extract . left2
    , pure     . extract . left2  . extract . left
    , pure     . extract . right2 . extract . left
    , pure     . extract . left2  . extract
    , pure     . extract . right2 . extract
    , pure     . extract . left2  . extract . right
    , pure     . extract . right2 . extract . right
    , nearest5 . extract . right2
    ] >>= ($ getUniverse2 u)

neighbours :: (Universe2 a) -> [a]
neighbours u =
    [ nearest3 . extract . left
    , pure     . extract . left  . extract
    , pure     . extract . right . extract
    , nearest3 . extract . right
    ] >>= ($ getUniverse2 u)

