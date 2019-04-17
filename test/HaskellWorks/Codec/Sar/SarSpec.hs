{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Codec.Sar.SarSpec
  ( spec
  ) where

import Control.Monad.IO.Class
import Data.Function
import HaskellWorks.Codec.Sar
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec                  (Spec, describe, it)

import qualified HaskellWorks.Codec.Sar.Gen as G
import qualified Hedgehog.Gen               as G
import qualified Hedgehog.Range             as R
import qualified System.IO                  as IO
import qualified System.Process             as IO
import qualified Test.Hspec                 as HS

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Codec.Sar.SarSpec" $ do
  it "raw round trip" $ requireProperty $ do
    rawEntries <- forAll $ G.list (R.linear 0 10) $ G.choice
      [ G.rawFile (R.linear 1 10) G.alpha
      , G.rawChunk (R.linear 1 10) (G.word8 R.constantBounded)
      ]
    unpackRawEntries (packRawEntries rawEntries) === rawEntries
  it "round trip" $ requireProperty $ do
    entries <- forAll $ G.list (R.linear 0 10) $ G.entry
      (R.linear 1  10)   G.alpha
      (R.linear 1 100)  (G.word8 R.constantBounded)
    rawEntries <- forAll $ pure $ entriesToRawEntries 10 entries
    rawEntriesToEntries rawEntries === entries
