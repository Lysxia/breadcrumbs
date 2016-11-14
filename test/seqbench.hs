import Criterion.Main
import Data.Foldable

import qualified Data.Breadcrumbs as B
import qualified Data.Breadcrumbs.DropList as D
import qualified Data.Breadcrumbs.Deque as Q
import qualified Data.Breadcrumbs.Seq as S
import qualified Data.Breadcrumbs.Flips as F
import qualified Data.Breadcrumbs.Caterpillar as C
import Data.Breadcrumbs.Test

main = defaultMain
  [ bench "TreeBuffer" $ seqbench B.empty B.push `whnf` length
  , bench "DropList" $ seqbench D.empty D.push `whnf` D.len
  , bench "Deque" $ seqbench Q.empty Q.push `whnf` Q.len
  , bench "Seq" $ seqbench S.empty S.push `whnf` S.len
  , bench "Flips" $ seqbench F.empty F.push `whnf` F.len
  , bench "Caterpillar" $ seqbench C.empty C.push `whnf` C.len
  ]

size = 10
iters = 1000

seqbench = appendStuff size iters
