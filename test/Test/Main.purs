module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.FoldableRec (foldMR, traverseRec_)

main :: Eff (console :: CONSOLE) Unit
main = do
  log "for"
  traverseRec_ logShow [0, 1, 2, 3, 4]

  log "foldMR"
  logShow =<< foldMR (\x y -> logShow y *> pure (x + y)) 0 [1, 2, 3, 4]
