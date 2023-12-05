{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE KindSignatures      #-}

module A where

import Control.Arrow (first)
import Data.Kind
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Sized as SU
import qualified Data.Vector as V

type Poly (v :: Type -> Type) (a :: Type) = v (SU.Vector 1 Word, a)
data Laurent (v :: Type -> Type) (a :: Type) =
  Laurent !(SU.Vector 1 Int) !(Poly v a)

f :: forall v a. (G.Vector v (SU.Vector 1 Word, a))
  => Poly v a -> Laurent v a
f = G.foldl' go (Laurent 0 (G.empty))
  where
    go :: Laurent v a -> (SU.Vector 1 Word, a) -> Laurent v a
    go (Laurent o p) (ps, _) = case doMonom ps of Laurent _ _ -> toLaurent o p

    doMonom :: SU.Vector 1 Word -> Laurent v a
    doMonom = SU.ifoldl' (\acc _ _ -> acc) (Laurent 0 G.empty)
{-# INLINE f #-}

toLaurent
  :: (G.Vector v (SU.Vector 1 Word, a))
  => SU.Vector 1 Int
  -> Poly v a
  -> Laurent v a
toLaurent off xs
  = Laurent (SU.zipWith const off minPow) ys
    where
      minPow = G.foldl'(\acc (x, _) -> SU.zipWith min acc x) (SU.replicate maxBound) xs
      ys = G.map (first (SU.zipWith const minPow)) xs
{-# INLINE toLaurent #-}
