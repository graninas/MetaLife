module GlossSteppable where

import Graphics.Gloss

class GlossSteppable a where
    step :: a -> a
