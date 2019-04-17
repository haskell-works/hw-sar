{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Codec.Sar
  ( packRawEntries
  , unpackRawEntries
  ) where

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

data Entry = Entry FilePath LBS.ByteString
  deriving (Eq, Show)

entriesToRawEntries :: Int -> [Entry] -> [RawEntry]
entriesToRawEntries n es = go es []
  where go :: [Entry] -> [RawEntry] -> [RawEntry]
        go []                          = id
        go (Entry filename payload:es) = (RawFile filename:) <> foldMap ((:) . RawChunk) (LBS.chunkBy n payload) <> go es

packRawEntries :: [RawEntry] -> LBS.ByteString
packRawEntries es = B.toLazyByteString $ mconcat (fmap packRawEntry es)

unpackRawEntries :: LBS.ByteString -> [RawEntry]
unpackRawEntries = go
  where go "" = []
        go lbs = case BG.runGetOrFail BG.maybeEntry lbs of
          Right (rs, _, RawError message) -> [RawError (message <> ": " <> show (LBS.take 8 lbs))]
          Right (rs, _, a       )         -> a:go rs
          Left  (_ , _, message )         -> [RawError ("Parse error: " <> message)]
