{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Posited where

import Data.Array
import Data.Bifunctor (first)
import Data.Maybe (listToMaybe)
import Text.Regex.Base
import Text.Regex.TDFA
import Text.Regex.TDFA.Common
import Text.Regex.TDFA.NewDFA.Engine
import qualified Text.Regex.TDFA.NewDFA.Tester as Tester
import Text.Regex.TDFA.NewDFA.Uncons

data Posited a = Posited { pos :: !Int, unPosited :: a }
  deriving (Foldable, Functor, Traversable, Eq, Show)

unPositedL :: Functor f => (a -> f b) -> Posited a -> f (Posited b)
unPositedL f (Posited k a) = Posited k <$> f a

posL :: Functor f => (Int -> f Int) -> Posited a -> f (Posited a)
posL f (Posited k a) = flip Posited a <$> f k

instance Semigroup a => Semigroup (Posited a) where
    Posited k a <> Posited _ b = Posited k (a <> b)

instance Extract a => Extract (Posited a) where
    before = fmap . before
    after i (Posited j a) = Posited (i+j) (after i a)
    empty = Posited 0 empty
    extract k@(i,_) (Posited j a) = Posited (i+j) (extract k a)

instance Uncons a => Uncons (Posited a) where
    uncons (Posited k a) = fmap (Posited (k+1)) <$> uncons a

instance (Extract a, Uncons a) => RegexLike Regex (Posited a) where
    matchOnce r s = listToMaybe (matchAll r s)
    matchAll r (Posited k s) = (fmap . first) (+ negate k) <$> execMatch r k '\n' s
    matchCount r s = length (matchAll r' s)
      where r' = r { regex_execOptions = (regex_execOptions r) {captureGroups = False} }
    matchTest = Tester.matchTest
    matchAllText regex source =
        let go :: Int -> Posited a -> [Array Int (Int, Int)] -> [Array Int (Posited a, (Int, Int))]
            go i t = i `seq` \ case
                [] -> []
                x:xs ->
                    let (off0,len0) = x!0
                        trans pair@(off,len) = (extract (off-i,len) t,pair)
                        t' = after (off0+(len0-i)) t
                    in fmap trans x : seq t' (go (off0+len0) t' xs)
        in go 0 source (matchAll regex source)
