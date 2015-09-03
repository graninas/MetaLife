module Samples where

import Sample



a :: BinaryLife c => c
a = binaryAlive

d :: BinaryLife c => c
d = binaryDead

cells', cells'', cells''', glider :: BinaryLife c => Sample c

cells' = Sample [[a, a, d, a, a]]

cells'' = Sample  [[a, a, a]]

glider = Sample [ [d, a, d]
         , [a, d, d]
         , [a, a, a] ]

cells''' = Sample [
          [d, d, d, d, a, d ]
        , [d, d, d, a, d, d ]
        , [d, d, d, a, a, a ]
        , [d, d, d, d, d, d ]
        , [d, d, d, d, d, d ]
        , [d, d, d, d, d, d ]
        , [d, d, d, d, d, d ]
        , [d, d, d, d, d, d ]
        , [a, d, a, a, a, a ]
        , [d, a, a, a, d, a ]
        , [a, d, d, a, a, a ]
        , [d, a, d, d, a, d ]
        , [a, d, d, a, a, d ]
        , [d, a, a, d, d, a ]
        , [a, d, a, a, a, a ]
        , [d, a, a, d, a, a ]
        , [a, d, a, d, a, a ]
        , [d, a, d, d, a, d ]
        , [a, d, a, a, d, d ]
        , [d, a, a, d, a, a ]
        , [a, d, d, a, d, d ]
        ]
        
        
testLine, testBlock :: BinaryLife c => Sample c
fillers :: BinaryLife c => Int -> Sample c

testLine  = Sample [ a : replicate 28 d ++ [a]]
testBlock = Sample [ replicate 14 d ++ [a, a] ++ replicate 14 d]
fillers n = Sample $ replicate n (replicate 30 d)

{-
testCells =  testLine
      ++ fillers 6
      ++ testLine
      ++ fillers 6
      ++ testBlock
      ++ testBlock
      ++ fillers 6
      ++ testLine
      ++ fillers 6
      ++ testLine
-}
