{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Codec.Sar
  ( packRawEntries
  , unpackRawEntries
  ) where

import Data.Word
import HaskellWorks.Codec.Sar.Internal.Pack
import HaskellWorks.Codec.Sar.Internal.RawEntry

import qualified Data.Binary.Get                     as BG
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Builder             as B
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import qualified HaskellWorks.Codec.Sar.Internal.Get as BG

data Entry = Entry FilePath LBS.ByteString
  deriving (Eq, Show)

packRawEntries :: [RawEntry] -> LBS.ByteString
packRawEntries es = B.toLazyByteString $ "![SARFILE]" <> mconcat (fmap packRawEntry es)

unpackRawEntries :: LBS.ByteString -> [RawEntry]
unpackRawEntries = go
  where go lbs = case BG.runGetOrFail BG.maybeEntry lbs of
          Right (rs, _, a       ) -> a:go rs
          Left  (_ , _, message ) -> [RawError ("Parse error: " <> message)]
