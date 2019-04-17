module HaskellWorks.Codec.Sar.Gen
  ( rawFile
  , rawChunk
  , entry
  ) where

import Data.Word
import HaskellWorks.Codec.Sar
import HaskellWorks.Codec.Sar.Internal.RawEntry
import Hedgehog                                 (MonadGen)

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Hedgehog.Gen         as G
import qualified Hedgehog.Range       as R

rawFile :: MonadGen m => R.Range Int -> m Char -> m RawEntry
rawFile r g = RawFile <$> G.string r g

rawChunk :: MonadGen m => R.Range Int -> m Word8 -> m RawEntry
rawChunk r g = RawChunk . LBS.fromStrict . BS.pack <$> G.list r g

entry :: MonadGen m => R.Range Int -> m Char -> R.Range Int -> m Word8 -> m Entry
entry rf gc rd gw = Entry
  <$> G.string rf gc
  <*> (LBS.fromStrict . BS.pack <$> G.list rd gw)
