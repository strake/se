{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Process (CreateProcess (..), StdStream (..), proc, readCreateProcessLazy, shell) where

import Prelude hiding (length)

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Filtrable
import Data.Foldable (Foldable (..), traverse_)
import Data.Functor.Compose (Compose (..))
import Data.ListLike (ListLikeIO)
import qualified Data.ListLike as ListLike
import GHC.IO.Exception (IOErrorType (..), ioe_type)
import System.IO hiding (hPutStr)
import System.IO.Unsafe
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, shell, terminateProcess, waitForProcess)
import System.Process.Common (ListLikeProcessIO (..), ProcessResult (..))
import System.Process.ByteString ()
import System.Process.ByteString.Lazy ()

readCreateProcessLazy
 :: (ProcessResult a b, ListLikeProcessIO a c) => CreateProcess -> a -> IO b
readCreateProcessLazy maker input = mask \restore -> do
    (inh, outh, errh, pid) <- createProcess maker
    onException
       (restore do
            -- fork off a thread to start consuming stdout
            -- Without unsafeIntereleaveIO the pid messsage gets stuck
            -- until some additional output arrives from the process.
            waitOut <- forkWait $ (pidf pid <>) <$> unsafeInterleaveIO
               ((readInterleaved . mapMaybe sequenceA)
                [(outf, outh), (errf, errh)] (codef <$> waitForProcess pid))
            flip writeInput input `traverse_` inh
            waitOut)
      (do terminateProcess pid; hClose `traverse_` Compose [inh, outh, errh]; waitForProcess pid)

readInterleaved
 :: ∀ a b c f. (ListLikeProcessIO a c, ProcessResult a b, Foldable f)
 => f (a -> b, Handle) -> IO b -> IO b
readInterleaved pairs finish = do
  res <- newEmptyMVar
  let -- Forked thread to read the input and send it to takeChunks via the MVar.
      readHandle :: (a -> b) -> Handle -> IO ()
      readHandle f h = do
        cs <- readChunks h
        -- If the type returned as stdout and stderr is lazy we need to force it here in the
        -- producer thread - I'm not exactly sure why.  And why is String lazy?
        -- when (lazy (undefined :: a)) (void cs)
        traverse_ (putMVar res . Right . f) cs
        hClose h
        putMVar res (Left h)
      takeChunks :: Int -> IO b
      takeChunks 0 = finish
      takeChunks openCount = takeChunk >>= takeMore openCount
      takeMore :: Int -> Either Handle b -> IO b
      takeMore openCount (Left h) = hClose h >> takeChunks (openCount - 1)
      takeMore openCount (Right x) = (<>) x <$> unsafeInterleaveIO (takeChunks openCount)
      takeChunk = takeMVar res `catch` (pure . Right . intf :: SomeException -> _)
  traverse_ (forkIO . uncurry readHandle) pairs
  takeChunks (length pairs)

-- | Write and flush process input, closing the handle when done.
-- Catch and ignore Resource Vanished exceptions — they just mean the
-- process exited before all of its output was read.
writeInput :: ListLikeIO a c => Handle -> a -> IO ()
writeInput inh input =
    ignoreResourceVanished do
      unless (ListLike.null input) do
        ListLike.hPutStr inh input
        hFlush inh
      hClose inh -- stdin has been fully written

-- | Wrapper for a process that provides a handler for the ResourceVanished exception
--
-- This is frequently an exception we wish to ignore, because many processes will deliberately
-- exit before they have read all of their input.
ignoreResourceVanished :: IO () -> IO ()
ignoreResourceVanished = handle \e -> unless (ioe_type e == ResourceVanished) (ioError e)

forkWait :: IO a -> IO (IO a)
forkWait a = do
  res <- newEmptyMVar
  (takeMVar res >>= either (throwIO :: SomeException -> _) pure) <$ mask \restore ->
      forkIO $ try (restore a) >>= putMVar res
