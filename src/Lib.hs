{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (Program (..), doProgram) where

import Control.Exception (throwIO)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (..))
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Char (isAlphaNum, isSpace)
import qualified Data.Char.Properties.BidiBrackets as UC
import Data.Ix (Ix)
import Data.Maybe (listToMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import Lens.Micro (set)
import System.Exit (ExitCode (..))
import Text.Regex.Base (makeRegexOptsM)
import qualified Text.Regex.Base as Regex
import Text.Regex.TDFA (Regex)
import qualified Text.Regex.TDFA as Regex
import Util

import Process (readCreateProcessLazy, shell)
import qualified Process

import Posited

doProgram :: MonadFail m => Program -> m (Text -> IO Text)
doProgram = fmap (\ f -> fmap unPosited . f . Posited 0) . go . programSource
  where
    go :: MonadFail m => [Char] -> m (Posited Text -> IO (Posited Text))
    go = dropWhile isSpace & \ case
        [] -> pure pure
        'p':pxs -> (\ k xs -> liftIO (Text.putStrLn (unPosited xs)) *> k xs) <$> go pxs
        's':pxs@(d:_) | Nothing <- UC.paired d ->
          [ withResetPos go
          | ((re, ψ), _flags) <- runStateT ((,) <$> parseDelimitedRegex <*> parsePostdelimitedSubst d) pxs
          , let go xs
                  | Just (Posited k xs, ms, zs) <- Regex.matchOnceText re xs =
                        [Posited k (xs <> ys' <> zs') | ys' <- ψ (unPosited . fst <$> ms), Posited _ zs' <- go zs]
                  | otherwise = pure xs ]
        'x':pxs ->
          [ withResetPos go
          | (re, pxs) <- parseDelimitedRegex `runStateT` pxs
          , kont <- go pxs
          , let go xs
                  | Just (xs, ms, zs) <- Regex.matchOnceText re xs, (0, (ys, _)):_ <- Array.assocs ms =
                        [xs <> ys' <> zs' | ys' <- withResetPos kont ys, zs' <- go zs]
                  | otherwise = pure xs ]
        'y':pxs ->
          [ withResetPos go
          | (re, pxs) <- parseDelimitedRegex `runStateT` pxs
          , kont <- go pxs
          , let go xs
                  | Just (xs, ms, zs) <- Regex.matchOnceText re xs, (0, (ys, _)):_ <- Array.assocs ms =
                        [xs' <> ys <> zs' | xs' <- withResetPos kont xs, zs' <- go zs]
                  | otherwise = kont xs ]
        '|':pxs -> pure $ traverse $ readCreateProcessLazy (shell pxs)
            { Process.std_in = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.Inherit } >=> \ case
            (ExitSuccess, xs, _ :: Text) -> pure xs
            (e, _, _) -> throwIO e
        pxs -> fail ("Failed to parse program: " ++ show pxs)

withResetPos :: Functor f => (Posited a -> f (Posited a)) -> Posited a -> f (Posited a)
withResetPos f (Posited k a) = set posL k <$> f (Posited 0 a)

parseDelimitedRegex :: MonadFail m => StateT [Char] m Regex
parseDelimitedRegex = do
    (dl, dr) <- StateT \ case
        [] -> fail "No regex"
        dl:xs -> [((dl, dr), xs) | dr <- case UC.paired dl of
            Nothing -> pure dl
            Just (dr, UC.O) -> pure dr
            Just (_, UC.C) -> fail ("Invalid opening delimiter: " ++ show dl)]
    lift . f =<< parseDelimitedHelper dl dr
  where
    f = makeRegexOptsM Regex.CompOption
          { Regex.caseSensitive = True
          , Regex.multiline = False
          , Regex.rightAssoc = True
          , Regex.newSyntax = True
          , Regex.lastStarGreedy = True
          } (Regex.ExecOption True)

type Subst m = Array Int Text -> m Text

parsePostdelimitedSubst :: (MonadFail m, MonadFail n) => Char -> StateT [Char] m (Subst n)
parsePostdelimitedSubst d = flip f <$> parseDelimitedHelper d d
  where
    f yss = go
      where
        go = \ case
            [] -> pure ""
            '\\':xs
              | (n, xs):_ <- reads xs, Just ys <- yss !? n -> (ys <>) <$> go xs
              | otherwise -> fail ("Invalid backreference: " ++ show xs)
            x:xs -> Text.cons x <$> go xs

parseDelimitedHelper :: MonadFail m => Char -> Char -> StateT [Char] m [Char]
parseDelimitedHelper dl dr = StateT (go "")
  where
    -- Mind the order! ◇(dl == dr)
    go _ [] = fail "Unterminated regex"
    go acc (x:xs)
      | dr == x = pure (acc, xs)
      | dl == x = fail ("Invalid character in regex: " ++ show x)
    go acc ('\\':x:xs)
      | False <- isAlphaNum x = go (acc ++ [x]) xs
    go acc (x:xs) = go (acc ++ [x]) xs

newtype Program = Program { programSource :: [Char] }

(!?) :: Ix k => Array k a -> k -> Maybe a
as !? k = listToMaybe [a | (k', a) <- Array.assocs as, k == k']
