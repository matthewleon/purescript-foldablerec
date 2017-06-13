module Bench.Main where

import Prelude
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Console (CONSOLE, log) 
import Data.Array ((..), length)
import Data.Foldable (traverse_)
import Data.FoldableRec (traverseRec_)

import Performance.Minibench (bench)

main :: Eff (console :: CONSOLE) Unit
main = do
  let smallArr = 1 .. 1000
      bigArr = 1 .. 10000
      noop = const (pure unit)

  log $ "foreachE (" <> show (length smallArr) <> ")"
  bench \_ -> unsafePerformEff (foreachE smallArr noop)

  log $ "foreachE (" <> show (length bigArr) <> ")"
  bench \_ -> unsafePerformEff (foreachE bigArr noop)

  log $ "traverse_ (" <> show (length smallArr) <> ")"
  bench \_ -> unsafePerformEff (traverse_ noop smallArr)

  -- stack explosion! log "traverse_"
  -- bench \_ -> unsafePerformEff (traverse_ noop arr)

  log $ "traverseRec_ (" <> show (length smallArr) <> ")"
  bench \_ -> unsafePerformEff (traverseRec_ noop smallArr)

  log $ "traverseRec_ (" <> show (length bigArr) <> ")"
  bench \_ -> unsafePerformEff (traverseRec_ noop bigArr)
