{-# LANGUAGE TupleSections, BangPatterns, FlexibleInstances #-}

module MetaLife where

import Universe
import UniverseNeighbours
import GlossSteppable
import GlossRenderable

import Sample

import Graphics.Gloss
import Control.Comonad

type Aliveness = Float
type Strength = Float
type InfluenceArea = Float
data Cell = Cell {
      aliveness :: !Aliveness
    , influenceArea :: !InfluenceArea
    , sterngth :: !Strength
    }

newtype MetaLife = MetaLife (Universe2 Cell)
    
instance GlossRenderable Cell where
    render = renderCell
    
--instance GlossSteppable MetaLife where
--    step (MetaLife u) = MetaLife $ u `stepWith` rule

instance GlossSteppable (Universe2 Cell) where
    step u = u `stepWith` rule

alive = 1.0
dead  = 0.0

aliveCell = Cell alive 1.0 1.0
deadCell = Cell dead 0.0 0.0

instance BinaryLife Cell where
    binaryAlive = aliveCell
    binaryDead = deadCell

isAlive :: Cell -> Bool
isAlive c = aliveness c == alive

toAlive c = c {aliveness = alive}
toDead  c = c {aliveness = dead}

rule :: Universe2 Cell -> Cell
rule u = let
    current = extract u
    nc = length $ filter isAlive (neighbours u) -- This can be optimized (use fold)
    in case nc of
        2         -> current
        3         -> toAlive current
        otherwise -> toDead current

alivenessRadius = 3.0

renderAlivenessCore :: Float -> Picture
renderAlivenessCore a | a == alive = Color green $ Circle alivenessRadius
                      | otherwise  = Color red   $ Circle alivenessRadius
renderInfluenceArea :: Float -> Float -> Picture
renderInfluenceArea ia s = Color black $ ThickCircle s ia

renderCell :: Cell -> Picture
renderCell (Cell a ia s) = Pictures [ renderAlivenessCore a
                                    , renderInfluenceArea ia s]
                                    
                                    
