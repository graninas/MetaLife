module UniverseRender where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Monoid

import GlossRenderable
import Universe
    
instance GlossRenderable c => GlossRenderable (Universe2 c) where
    render = toPicture . toList2

side = 30.0

toPicture :: GlossRenderable c => [[c]] -> Picture
toPicture picss = (foldr1 mappend . foldr1 mappend) $ map toPicture' (zip [screenXShift..] picss)
  where
    screenXShift = (length picss) `div` (-2)
    screenYShift = screenXShift
    toPicture' (j, pics) = map (transCellX j) (zip [screenYShift..] pics)


transCellX :: GlossRenderable c => Int -> (Int, c) -> Picture
transCellX j (i, c) = Translate ((fromIntegral i) * side) ((fromIntegral j) * side) (render c)

