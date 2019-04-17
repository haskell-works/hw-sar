{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Codec.Sar.Internal.Pack
  ( packRawEntry
  ) where

import HaskellWorks.Codec.Sar.Internal.RawEntry

import qualified Data.Binary.Get         as BG
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T

packRawEntry :: RawEntry -> B.Builder
packRawEntry (RawFile filename) = "![file]"  <> B.word64LE (fromIntegral (   length filename)) <> B.byteString (T.encodeUtf8 (T.pack filename))
packRawEntry (RawChunk payload) = "![chunk]" <> B.word64LE (fromIntegral (BS.length payload )) <> B.byteString payload
