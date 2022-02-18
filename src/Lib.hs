{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (Program (..), doProgram) where

import Control.Applicative (Alternative (..), liftA2)
import Control.Exception (throwIO)
import Control.Monad (MonadPlus (..), (>=>), join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (..), modify)
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Bool (bool)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS (putStrLn)
import qualified Data.ByteString.Lazy.UTF8 as BS
import Data.Char (isAlpha, isSpace)
import qualified Data.Char.Properties.BidiBrackets as UC
import Data.Filtrable (spanJust)
import Data.Foldable (foldl')
import Data.Function (on)
import Data.Ix (Ix)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe, listToMaybe)
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

doProgram :: MonadFail m => Program -> m (ByteString -> IO ByteString)
doProgram = fmap (\ f -> fmap unPosited . f . Posited 0) . go . programSource
  where
    go :: MonadFail m => [Char] -> m (Posited ByteString -> IO (Posited ByteString))
    go = dropWhile isSpace & \ case
        [] -> pure pure
        'l':pxs@(d:_) | Nothing <- UC.paired d ->
          [ traverse $ pure . (on tr parseTrSeq) s t
          | ((s, t), _) <- runStateT ((join . liftA2) (,) $ parseDelimitedHelper d d) pxs ]
        'p':pxs -> (\ k xs -> liftIO (BS.putStrLn (unPosited xs)) *> k xs) <$> go pxs
        's':pxs@(d:_) | Nothing <- UC.paired d ->
          [ withResetPos go
          | ((re, ψ), _flags) <- runStateT ((,) <$> parseDelimitedRegex <*> parsePostdelimitedSubst d) pxs
          , let go xs
                  | Just (Posited k xs, ms, zs) <- Regex.matchOnceText re xs =
                        [Posited k (xs <> ys' <> zs') | ys' <- ψ (unPosited . fst <$> ms), Posited _ zs' <- go zs]
                  | otherwise = pure xs ]
        'u':pxs -> helperReK pxs \ re kont -> bool pure kont =<< Regex.matchTest re
        'v':pxs -> helperReK pxs \ re kont -> bool kont pure =<< Regex.matchTest re
        'x':pxs -> helperReK pxs \ re kont ->
            let go xs
                  | Just (xs, ms, zs) <- Regex.matchOnceText re xs, (0, (ys, _)):_ <- Array.assocs ms =
                        [xs <> ys' <> zs' | ys' <- withResetPos kont ys, zs' <- go zs]
                  | otherwise = pure xs
            in go
        'y':pxs -> helperReK pxs \ re kont ->
            let go xs
                  | Just (xs, ms, zs) <- Regex.matchOnceText re xs, (0, (ys, _)):_ <- Array.assocs ms =
                        [xs' <> ys <> zs' | xs' <- withResetPos kont xs, zs' <- go zs]
                  | otherwise = kont xs
            in go
        '|':pxs -> pure $ traverse $ readCreateProcessLazy (shell pxs)
            { Process.std_in = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.Inherit } >=> \ case
            (ExitSuccess, xs, _ :: ByteString) -> pure xs
            (e, _, _) -> throwIO e
        pxs -> fail ("Failed to parse program: " ++ show pxs)

    helperReK pxs f =
      [ withResetPos $ f re kont
      | (re, pxs) <- parseDelimitedRegex `runStateT` pxs
      , kont <- go pxs ]

tr :: [Char] -> [Char] -> ByteString -> ByteString
tr s t = BS.fromString . fmap (fromMaybe <*> flip IntMap.lookup r . fromEnum) . BS.toString
  where
    r = IntMap.fromList (zip (fromEnum <$> s) t)

parseTrSeq :: [Char] -> [Char]
parseTrSeq = \ case
    [] -> []
    x:'-':y:xs -> [x..y] ++ parseTrSeq xs
    x:xs -> x:parseTrSeq xs

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

type Subst m = Array Int ByteString -> m ByteString

parsePostdelimitedSubst :: (MonadFail m, MonadFail n) => Char -> StateT [Char] m (Subst n)
parsePostdelimitedSubst d = flip f <$> parseDelimitedHelper d d
  where
    f yss = go
      where
        go = \ case
            [] -> pure ""
            '\\':xs
              | Just (ys, xs) <-
                [ (ys, xs)
                | (n, xs) <- runStateT parseOptionallyBracketedNatural xs
                , ys <- yss !? n ] -> (ys <>) <$> go xs
              | otherwise -> fail ("Invalid backreference: " ++ show xs)
            x:xs -> (BS.fromString [x] <>) <$> go xs

parseNatural :: (Alternative f, Num b) => StateT [Char] f b
parseNatural = StateT $
    dropWhile isSpace & spanJust digit & \ case
        ([], _) -> empty
        (ys, xs) -> pure (fromDigits 10 ys, xs)

parseOptionallyBracketedNatural :: (MonadPlus f, Num b) => StateT [Char] f b
parseOptionallyBracketedNatural = StateT \ case
    '{':xs -> flip runStateT xs do
        n <- parseNatural
        modify (dropWhile isSpace)
        StateT \ case
            '}':xs -> pure (n, xs)
            _ -> empty
    xs -> runStateT parseNatural xs

fromDigits :: (Integral a, Num b) => Word -> [a] -> b
fromDigits r = foldl' (\ n w -> fromIntegral r*n + fromIntegral w) 0

parseDelimitedHelper :: MonadFail m => Char -> Char -> StateT [Char] m [Char]
parseDelimitedHelper dl dr = StateT (go "")
  where
    -- Mind the order! ◇(dl == dr)
    go _ [] = fail "Unterminated regex"
    go acc (x:xs)
      | dr == x = pure (acc, xs)
      | dl == x = fail ("Invalid character in regex: " ++ show x)
    go acc ('\\':xs'@(x:xs))
      | not (isAlpha x) = go (acc ++ [x]) xs
      | isAlpha x = runStateT (pure <$> parseEscapeSeq) xs'
    go acc ('\\':[]) = fail "Invalid escape sequence: <empty>"
    go acc (x:xs) = go (acc ++ [x]) xs

parseEscapeSeq :: MonadFail m => StateT [Char] m Char
parseEscapeSeq = StateT \ case
    -- We not yet recognize any escape sequences.
    xs -> fail ("Invalid escape sequence: " ++ xs)

newtype Program = Program { programSource :: [Char] }

(!?) :: Ix k => Array k a -> k -> Maybe a
as !? k = listToMaybe [a | (k', a) <- Array.assocs as, k == k']
