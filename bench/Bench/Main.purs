module Bench.Main where

import Prelude
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Console (CONSOLE, log) 
import Control.Monad.ST (pureST)
import Data.Array ((..), (!!), length, fromFoldable)
import Data.Array.ST (emptySTArray, pushSTArray, unsafeFreeze)
import Data.Array.ST.Iterator (iterator, pushAll)
import Data.Foldable (traverse_)
import Data.FoldableRec (class FoldableMR, traverseRec_)
import Data.Maybe (Maybe)

import Performance.Minibench (bench)

main :: Eff (console :: CONSOLE) Unit
main = do
  let smallArr = 1 .. 1000
      bigArr = 1 .. 10000
      noop = const (pure unit)

  log "noop traversals"
  log "==============="

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

  log ""

  log "array-building traversals"

  log $ "Array.fromFoldable (" <> show (length smallArr) <> ")"
  bench \_ -> void $ fromFoldable smallArr

  log $ "Array.fromFoldable (" <> show (length bigArr) <> ")"
  bench \_ -> void $ fromFoldable bigArr

  log $ "Array from iterator (" <> show (length smallArr) <> ")"
  bench \_ -> void $ buildArrayIterator (smallArr !! _)

  log $ "Array from iterator (" <> show (length bigArr) <> ")"
  bench \_ -> void $ buildArrayIterator (bigArr !! _)

  log $ "traverseRec_ buildArray (" <> show (length smallArr) <> ")"
  bench \_ -> void $ buildArrayMR smallArr

  log $ "traverseRec_ buildArray (" <> show (length bigArr) <> ")"
  bench \_ -> void $ buildArrayMR bigArr

  where

  buildArrayIterator :: forall a. (Int -> Maybe a) -> Array a
  buildArrayIterator index = pureST do
    iter <- iterator index
    arr <- emptySTArray
    pushAll iter arr
    unsafeFreeze arr

  buildArrayMR :: forall f a. FoldableMR f => f a -> Array a
  buildArrayMR xs = pureST do
    arr <- emptySTArray
    traverseRec_ (void <<< pushSTArray arr) xs
    unsafeFreeze arr
