{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Tar.TarSpec
  ( spec
  ) where

import Control.Monad.IO.Class
import Data.Function
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec                  (Spec, describe, it)

import qualified System.IO      as IO
import qualified System.Process as IO
import qualified Test.Hspec     as HS

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Tar.TarSpec" $ do
  it "create" $ requireTest $ do
    True === True
