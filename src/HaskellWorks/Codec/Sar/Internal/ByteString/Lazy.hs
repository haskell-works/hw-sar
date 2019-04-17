module HaskellWorks.Codec.Sar.Internal.ByteString.Lazy
  ( chunkBy
  ) where

import qualified Data.ByteString.Lazy as LBS

chunkBy :: Int -> LBS.ByteString -> [LBS.ByteString]
chunkBy n lbs = case LBS.splitAt (fromIntegral n) lbs of
  (as, bs) -> if LBS.null as
    then []
    else if LBS.null bs
      then [as]
      else as:chunkBy n bs
