{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Codec.Sar
  ( Entry(..)
  , packRawEntries
  , unpackRawEntries
  , rawEntriesToEntries
  , entriesToRawEntries
  ) where

import Control.Monad
import Data.Word
import HaskellWorks.Codec.Sar.Internal.Pack
import HaskellWorks.Codec.Sar.Internal.RawEntry

import qualified Data.Binary.Get                                 as BG
import qualified Data.ByteString                                 as BS
import qualified Data.ByteString.Builder                         as B
import qualified Data.ByteString.Lazy                            as LBS
import qualified Data.Text                                       as T
import qualified Data.Text.Encoding                              as T
import qualified HaskellWorks.Codec.Sar.Internal.ByteString.Lazy as LBS
import qualified HaskellWorks.Codec.Sar.Internal.Get             as BG
import qualified System.Directory                                as IO
import qualified System.Directory.Internal                       as IO
import qualified System.IO                                       as IO

data Entry = Entry FilePath LBS.ByteString
  deriving (Eq, Show)

entriesToRawEntries :: Int -> [Entry] -> [RawEntry]
entriesToRawEntries n es = go es []
  where go :: [Entry] -> [RawEntry] -> [RawEntry]
        go []                          = id
        go (Entry filename payload:es) = (RawFile filename:) <> foldMap ((:) . RawChunk) (LBS.chunkBy n payload) <> go es

rawEntriesToEntries :: [RawEntry] -> [Entry]
rawEntriesToEntries res = go res []
  where go  :: [RawEntry]
            -> [Entry] -> [Entry]
        go (RawFile filename:res) = gogo filename mempty res
        go (RawChunk payload:res) = error "Chunk without file"
        go (RawError message:res) = error $ "ERROR: " <> message
        go []                     = id

        gogo :: FilePath -> B.Builder -> [RawEntry] -> [Entry] -> [Entry]
        gogo filename  b (RawChunk payload:res)   =                                               gogo filename (b <> B.lazyByteString payload) res
        gogo filename0 b (RawFile  filename1:res) = (Entry filename0 (B.toLazyByteString b):) <>  gogo filename1 mempty res
        gogo filename  b []                       = (Entry filename  (B.toLazyByteString b):)

packRawEntries :: [RawEntry] -> LBS.ByteString
packRawEntries es = B.toLazyByteString $ mconcat (fmap packRawEntry es)

unpackRawEntries :: LBS.ByteString -> [RawEntry]
unpackRawEntries = go
  where go "" = []
        go lbs = case BG.runGetOrFail BG.maybeEntry lbs of
          Right (rs, _, RawError message) -> [RawError (message <> ": " <> show (LBS.take 8 lbs))]
          Right (rs, _, a       )         -> a:go rs
          Left  (_ , _, message )         -> [RawError ("Parse error: " <> message)]
