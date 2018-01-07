module SSA.Temp
    ( new
    , create
    , get
    ) where

import Control.Monad.ST
import Data.STRef

data Dispenser = Dispenser Int

data Temp = Temp Int deriving (Eq)
instance Show Temp where
    show (Temp i) = "t" ++ show i

new = Dispenser 0
create (Dispenser i) = (i, Dispenser $ i + 1)
get (Temp i) = i
