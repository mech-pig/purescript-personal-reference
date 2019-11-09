module Test.MonadTransformers.Spec where

import Prelude
import Data.List (List(..), (:))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import MonadTransformers (Direction(..), move, Step(..), runRoverProgram, updateRoverState)


spec :: Spec Unit
spec =
  describe "Test.MonadTransformers" do

    describe "move" do

      it "returns origin if step is Rest" $
        (move Rest { x:  0, y:  0 }) `shouldEqual` { x:  0, y:  0 }

      it "returns origin if step is a Move in any direction and distance = 0" do
        (move (Move Up     0) { x:  0, y:  0 }) `shouldEqual` { x:  0, y:  0 }
        (move (Move Right  0) { x:  0, y:  0 }) `shouldEqual` { x:  0, y:  0 }
        (move (Move Down   0) { x:  0, y:  0 }) `shouldEqual` { x:  0, y:  0 }
        (move (Move Left   0) { x:  0, y:  0 }) `shouldEqual` { x:  0, y:  0 }

      it "translates origin if step is a Move in any direction and distance ≠ 0" do
        (move (Move Up      1) { x:  0, y:  0 }) `shouldEqual` { x:  0, y:  1 }
        (move (Move Up   (-1)) { x:  0, y:  0 }) `shouldEqual` { x:  0, y: -1 }
        (move (Move Up      2) { x:  0, y:  0 }) `shouldEqual` { x:  0, y:  2 }
        (move (Move Up      1) { x:  1, y:  2 }) `shouldEqual` { x:  1, y:  3 }

        (move (Move Right    1) { x:  0, y:  0 }) `shouldEqual` { x:  1, y:  0 }
        (move (Move Right (-1)) { x:  0, y:  0 }) `shouldEqual` { x: -1, y:  0 }
        (move (Move Right    2) { x:  0, y:  0 }) `shouldEqual` { x:  2, y:  0 }
        (move (Move Right    1) { x:  1, y:  2 }) `shouldEqual` { x:  2, y:  2 }

        (move (Move Down     1) { x:  0, y:  0 }) `shouldEqual` { x:  0, y: -1 }
        (move (Move Down  (-1)) { x:  0, y:  0 }) `shouldEqual` { x:  0, y:  1 }
        (move (Move Down     2) { x:  0, y:  0 }) `shouldEqual` { x:  0, y: -2 }
        (move (Move Down     1) { x:  1, y:  2 }) `shouldEqual` { x:  1, y:  1 }

        (move (Move Left     1) { x:  0, y:  0 }) `shouldEqual` { x: -1, y:  0 }
        (move (Move Left  (-1)) { x:  0, y:  0 }) `shouldEqual` { x:  1, y:  0 }
        (move (Move Left     2) { x:  0, y:  0 }) `shouldEqual` { x: -2, y:  0 }
        (move (Move Left     1) { x:  1, y:  2 }) `shouldEqual` { x:  0, y:  2 }

    describe "updateRoverState" do

      it "returns same state if step is Rest" $
        updateRoverState Rest { currentPosition: { x: 0, y: 0 } }
        `shouldEqual`
        { currentPosition: { x: 0, y: 0 } }

      it "updates the rover's current position in case of a Move step according to the direction and the distance" $
        updateRoverState (Move Up 1) { currentPosition: { x: 2, y: 1 } }
        `shouldEqual`
        { currentPosition: { x: 2, y: 2 } }

    describe "runRoverProgram" do

      it "executes a set of instruction given an initial state and returns the final state" do
        executeTest roverConfig { currentPosition: { x: 0, y: 0 } } { currentPosition: { x: -1, y: 3 } }
        executeTest roverConfig { currentPosition: { x: 1, y: -1 } } { currentPosition: { x: 0, y: 2 } }
        where
          executeTest config initialState expectedState = do
            finalState <- runRoverProgram config initialState
            finalState `shouldEqual` expectedState
          roverConfig = { instructions: ((Move Up 3) : (Move Left 1) : Nil) }
