module Data.FoldableRec where

import Prelude
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM2)

class FoldableMR f where
  foldMR :: forall m a b. MonadRec m => (a -> b -> m a) -> a -> f b -> m a

traverseRec_
  :: forall a f m
   . MonadRec m
  => FoldableMR f
  => (a -> m Unit)
  -> f a
  -> m Unit
traverseRec_ f = foldMR (const f) unit

forRec_
  :: forall a f m
   . MonadRec m
  => FoldableMR f
  => f a
  -> (a -> m Unit)
  -> m Unit
forRec_ = flip traverseRec_

-- commented out lines below due to some instancesigs issues
-- https://github.com/purescript/purescript/issues/2941
instance foldableMRArray :: FoldableMR Array where
  foldMR :: forall m a b. MonadRec m => (a -> b -> m a) -> a -> Array b -> m a
  foldMR opEffect accum xs = tailRecM2 go accum 0 
    where
    -- go :: a -> Int -> m (Step {a :: a, b :: Int} a)
    go accum' index = do
      accum'' <- opEffect accum' (indexArray index)
      pure $ if   index >= maxIndex
             then Done accum''
             else Loop {a: accum'', b: index + 1}

    --indexArray :: Int -> b
    indexArray = arrayIndexImpl xs

    maxIndex :: Int
    maxIndex = arrayLengthImpl xs - 1

foreign import arrayIndexImpl :: forall a. Array a -> Int -> a
foreign import arrayLengthImpl :: forall a. Array a -> Int
