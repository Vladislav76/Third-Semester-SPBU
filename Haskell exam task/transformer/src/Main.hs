module Main where

import Raise.Raise
import Raise.RaiseR
import Raise.RaiseE
import Raise.RaiseCC
import Raise.RaiseFinal
import Raise.RaisePromote

import Common.MonadRaise
import Common.Transformer

import Control.Monad.State

main :: IO ()
main = print "Hello, World! I want to believe that all works..."