module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
import Control.Comonad
import Control.Applicative
import Control.Parallel.Strategies
import Data.Monoid


import Universe
import MetaLife
import GlossSteppable
import GlossRenderable
import UniverseRender
import Sample
import Samples

data Mode = Play | Simulate
  deriving (Show, Read)


renderModel :: GlossRenderable a => a -> Picture
renderModel = render

stepModel :: GlossSteppable a => ViewPort -> Float -> a -> a
stepModel _ _ = step

control :: GlossSteppable a => Event -> a -> a
control (EventKey (SpecialKey KeySpace) Down _ _) us = step us
control _ us = us

run Play model iters ips s = play windowCfg white ips
                             model
                             renderModel
                             control
                             (const id)
run Simulate model iters ips s = simulate windowCfg white ips
                             model
                             renderModel
                             stepModel



configFile = "config.txt"
windowCfg = InWindow "Cellular automata" (1024, 768) (500, 300)
totalIters = 100
itersPerSec = 3


main = do


    let metaGlider = glider :: Sample Cell
    let field = fromList2 10 deadCell (fromSample metaGlider)
    run Simulate field totalIters itersPerSec 60



