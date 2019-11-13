module Data.SDM.Index where

import Data.SDM.VectorSpace
import Data.Void

-- we bind names to SparseVectors

data SemanticVector i v = SV { svK :: SparseVector i v
                             , svE :: SparseVector i v
                             }

