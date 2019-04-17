module HaskellWorks.Codec.Sar.Internal.RawEntry
  ( RawEntry(..)
  , RawEntryType(..)
  ) where

import qualified Data.ByteString as LBS

data RawEntry
  = RawFile FilePath
  | RawChunk LBS.ByteString
  | RawError String
  deriving (Eq, Show)

data RawEntryType
  = RawEntryTypeFile
  | RawEntryTypeChunk
  | RawEntryTypeError String
  deriving (Eq, Show)
