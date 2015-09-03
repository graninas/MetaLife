module ParallelUniverse where
import Control.Parallel.Strategies
import Control.DeepSeq

parNeighbours :: NFData a => ((Universe2 a) -> [a]) -> (Universe2 a) -> [a]
parNeighbours ns u = runEval $ parList rpar (force $ ns u)



