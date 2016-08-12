{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad (forM_)
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BSB
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           GHC.IO.Handle (hSetBinaryMode, hSetBuffering, BufferMode(..))
import           System.Environment (getArgs)
import           System.IO (stdin, stdout)


main :: IO ()
main = do
  cliArgs <- getArgs

  if ((not.null) cliArgs)
  then putStrLn "Reads from STDIN, writest to STDOUT, output is same as from 'sort | uniq -c | sort -n'"
  else runMain


runMain :: IO ()
runMain = do
  hSetBinaryMode stdin True
  hSetBuffering stdin (BlockBuffering Nothing)

  hSetBinaryMode stdout True
  hSetBuffering stdout (BlockBuffering Nothing)

  l2c <- stdinLines $$ countLines
  let orderedL2c = sortOn (snd) $ Map.toList l2c


  forM_ orderedL2c $ \(l,c) -> do
    BSB.hPutBuilder stdout $ (BSB.integerDec c) <> (BSB.byteString "\t") <> (BSB.byteString l) <> (BSB.byteString "\n")


countLines :: (MonadIO m)
           => Consumer ByteString m (Map ByteString Integer)
countLines = CL.fold addLine Map.empty
  where addLine !l2c !l = Map.insertWith (+) l 1 l2c


stdinLines :: (MonadIO m)
           => Producer m ByteString
stdinLines = do
  CB.sourceHandle stdin =$= CB.lines

