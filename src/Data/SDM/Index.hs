module Data.SDM.Index where

import Data.SDM.VectorSpace
import Data.Void

import Grenade

{-
hack up the use case where we use SDM to train the semantic vectors of some text and use
this as input to a RNN (LSTM) and see how performance differs for a 1-hot representation
-}

type Binary = Void

-- | TODO
-- Use shakespeare example
-- Pre train semantic space...
-- define LSTM network 
-- export vectors to input layer of LSTM

-- try sparse bits embedding

data SemanticVector1 i v = SV { svK :: SparseVector Int Binary
                              , svV :: SparseVector Int Binary
                              }

