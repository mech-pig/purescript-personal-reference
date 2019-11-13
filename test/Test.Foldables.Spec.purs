module Test.Foldables.Spec where

import Prelude

import Data.Maybe (Maybe(..))
import Global (infinity)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Foldables (positiveOnly, compress)


spec :: Spec Unit
spec =
  describe "Test.Foldables.Spec" do

    describe "compress" do

      it "returns Nothing if minValue > maxValue" $
        compress 1.0 0.0 [] `shouldEqual` Nothing

      it "returns array in input if it's empty" $
        compress 0.0 0.0 [] `shouldEqual` Just []

      it "floors values to minValue if they are below it (> 0)" $
        compress 5.0 infinity [2.0, 3.0, 4.0, 5.0] `shouldEqual` Just [5.0, 5.0, 5.0, 5.0]

      it "floors values to minValue if they are below it (< 0)" $
        compress (-2.0) infinity [-2.0, -3.0, -4.0, -5.0] `shouldEqual` Just [-2.0, -2.0, -2.0, -2.0]

      it "ceils values to maxValue if they are above it (> 0)" $
        compress (-infinity) 2.0 [2.0, 3.0, 4.0, 5.0] `shouldEqual` Just [2.0, 2.0, 2.0, 2.0]

      it "ceils values to maxValue if they are above it (< 0)" $
        compress (-infinity) (-5.0) [-2.0, -3.0, -4.0, -5.0] `shouldEqual` Just [-5.0, -5.0, -5.0, -5.0]

    describe "positiveOnly" do

      it "returns empty array if empty array is passed as input" $
        positiveOnly [] `shouldEqual` []

      it "removes entries equal to 0 from input array" $
        positiveOnly [0.0, 0.0, 0.0] `shouldEqual` []

      it "removes negative entries from input array" $
        positiveOnly [-1.0, -3.0, -5.0] `shouldEqual` []

      it "return input array without non positive entries" $
        positiveOnly [-1.0, 0.0, 1.0, 0.1, -3.0, 2.3] `shouldEqual` [1.0, 0.1, 2.3]
