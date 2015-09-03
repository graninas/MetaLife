module GlossRenderable where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


class GlossRenderable a where
    render :: a -> Picture



