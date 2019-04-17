{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Codec.Sar.Internal.Get
  ( lit
  , maybeEntry
  ) where

import Control.Applicative
import HaskellWorks.Codec.Sar.Internal.RawEntry

import qualified Data.Binary.Get      as BG
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T

lit :: BS.ByteString -> BG.Get ()
lit bs = do
  text <- BG.getByteString (fromIntegral (BS.length bs))
  if text == bs
    then return ()
    else fail $ "Failed to match literal " <> show text

maybeEntry :: BG.Get RawEntry
maybeEntry = do
  rawEntryType <- (RawEntryTypeFile  <$ lit "![file]")
              <|> (RawEntryTypeChunk <$ lit "![chunk]")
              <|> pure (RawEntryTypeError "Invalid entry type")
  case rawEntryType of
    RawEntryTypeChunk -> do
      sz <- BG.getWord64le
      RawChunk <$> BG.getByteString (fromIntegral sz)
    RawEntryTypeFile -> do
      sz <- BG.getWord64le
      RawFile . T.unpack . T.decodeUtf8  <$> BG.getByteString (fromIntegral sz)
    RawEntryTypeError msg -> return (RawError msg)
