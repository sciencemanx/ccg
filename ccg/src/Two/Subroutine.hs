module Two.Subroutine 
    ( Subroutine(..)
    , Program(..)
    ) where

import Common

import qualified Data.Map as Map

import Two.Cfg

type SubMap = Map.Map Addr Subroutine

data Subroutine = Sub Addr Cfg deriving (Show, Eq)
data Program = Prog SubMap deriving (Show, Eq)
