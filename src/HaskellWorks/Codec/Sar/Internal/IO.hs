module HaskellWorks.Codec.Sar.Internal.IO
  ( paths
  , paths'
  ) where

import Control.Monad

import qualified System.Directory          as IO
import qualified System.Directory.Internal as IO
import qualified System.IO                 as IO

paths :: FilePath -> IO [FilePath]
paths filePath = fmap ($ []) (paths' filePath)

paths' :: FilePath -> IO ([FilePath] -> [FilePath])
paths' filePath = do
  ps <- IO.listDirectory filePath

  rps <- forM ps $ \p' -> do
    let p = prependParent filePath p'
    m <- IO.getFileMetadata p

    let ft = IO.fileTypeFromMetadata m
    case ft of
      IO.File      -> return (p:)
      IO.Directory -> paths' p
      _            -> error $ "error: todo implement for " <> show ft

  return (mconcat rps)

  where prependParent fp e = if fp == "."
          then e
          else fp ++ "/" ++ e
