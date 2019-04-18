module HaskellWorks.Codec.Sar.Internal.Entry
  ( Entry(..)
  ) where

import qualified Data.ByteString.Lazy as LBS

data Entry = Entry FilePath LBS.ByteString
  deriving (Eq, Show)
