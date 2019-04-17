{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Codec.Sar.Internal.Pack
  ( packRawEntry
  ) where

import HaskellWorks.Codec.Sar.Internal.RawEntry

import qualified Data.Binary.Get         as BG
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T

packLazyByteString :: LBS.ByteString -> B.Builder
packLazyByteString bs = B.word64LE (fromIntegral (LBS.length bs )) <> B.lazyByteString bs

packRawEntry :: RawEntry -> B.Builder
packRawEntry (RawFile filename) = "![file]"  <> packLazyByteString (LBS.fromStrict (T.encodeUtf8 (T.pack filename)))
packRawEntry (RawChunk payload) = "![chunk]" <> packLazyByteString payload
