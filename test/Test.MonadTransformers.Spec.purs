module Test.MonadTransformers.Spec where

import Prelude
import Data.List (List(..), (:))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import MonadTransformers (Direction(..), move, Step(..), runRoverProgram, updateRoverState)


spec :: Spec Unit
spec =
  describe "Test.MonadTransformers" do

    describe "Step" do

      let testStepShowInstance = (\desc input expected ->
        it ("implements instance for Show (constructor: " <> desc <> ")") $
          show input `shouldEqual` expected
      )

      testStepShowInstance "Rest" Rest "Rest"
      testStepShowInstance "Move Up Distance" (Move Up 123) "Move Up 123"
      testStepShowInstance "Move Right Distance" (Move Right 123) "Move Right 123"
      testStepShowInstance "Move Down Distance" (Move Down 123) "Move Down 123"
      testStepShowInstance "Move Left Distance" (Move Left 123) "Move Left 123"


    describe "move" do

      let buildTestCase = (\origin step expectedPosition ->
        move step origin `shouldEqual` expectedPosition
      )

      let testRestStepReturnsOrigin = (\origin ->
        it ("returns origin if step is Rest (origin: " <> show origin <> ")") $
          buildTestCase origin Rest origin
      )

      testRestStepReturnsOrigin { x:  0, y:  0 }
      testRestStepReturnsOrigin { x:  0, y:  1 }
      testRestStepReturnsOrigin { x:  1, y:  0 }
      testRestStepReturnsOrigin { x:  1, y:  1 }
      testRestStepReturnsOrigin { x: -1, y: -1 }
      testRestStepReturnsOrigin { x:  0, y: -1 }

      let testReturnsOriginIfMoveOfDistanceZero = (\origin step ->
        it (
          "returns origin if step is a Move with distance = 0 (step: "
            <> show step <> ", origin: " <> show origin <> ")"
        ) $ buildTestCase origin step origin
      )

      testReturnsOriginIfMoveOfDistanceZero { x:  0, y:  0 } (Move Up    0)
      testReturnsOriginIfMoveOfDistanceZero { x:  0, y:  1 } (Move Up    0)
      testReturnsOriginIfMoveOfDistanceZero { x:  1, y:  0 } (Move Up    0)
      testReturnsOriginIfMoveOfDistanceZero { x:  1, y:  1 } (Move Up    0)
      testReturnsOriginIfMoveOfDistanceZero { x: -1, y:  0 } (Move Up    0)
      testReturnsOriginIfMoveOfDistanceZero { x:  0, y: -1 } (Move Up    0)
      testReturnsOriginIfMoveOfDistanceZero { x: -1, y: -1 } (Move Up    0)
      testReturnsOriginIfMoveOfDistanceZero { x:  0, y:  0 } (Move Right 0)
      testReturnsOriginIfMoveOfDistanceZero { x:  0, y:  1 } (Move Right 0)
      testReturnsOriginIfMoveOfDistanceZero { x:  1, y:  0 } (Move Right 0)
      testReturnsOriginIfMoveOfDistanceZero { x:  1, y:  1 } (Move Right 0)
      testReturnsOriginIfMoveOfDistanceZero { x: -1, y:  0 } (Move Right 0)
      testReturnsOriginIfMoveOfDistanceZero { x:  0, y: -1 } (Move Right 0)
      testReturnsOriginIfMoveOfDistanceZero { x: -1, y: -1 } (Move Right 0)
      testReturnsOriginIfMoveOfDistanceZero { x:  0, y:  0 } (Move Down  0)
      testReturnsOriginIfMoveOfDistanceZero { x:  0, y:  1 } (Move Down  0)
      testReturnsOriginIfMoveOfDistanceZero { x:  1, y:  0 } (Move Down  0)
      testReturnsOriginIfMoveOfDistanceZero { x:  1, y:  1 } (Move Down  0)
      testReturnsOriginIfMoveOfDistanceZero { x: -1, y:  0 } (Move Down  0)
      testReturnsOriginIfMoveOfDistanceZero { x:  0, y: -1 } (Move Down  0)
      testReturnsOriginIfMoveOfDistanceZero { x: -1, y: -1 } (Move Down  0)
      testReturnsOriginIfMoveOfDistanceZero { x:  0, y:  0 } (Move Left  0)
      testReturnsOriginIfMoveOfDistanceZero { x:  0, y:  1 } (Move Left  0)
      testReturnsOriginIfMoveOfDistanceZero { x:  1, y:  0 } (Move Left  0)
      testReturnsOriginIfMoveOfDistanceZero { x:  1, y:  1 } (Move Left  0)
      testReturnsOriginIfMoveOfDistanceZero { x: -1, y:  0 } (Move Left  0)
      testReturnsOriginIfMoveOfDistanceZero { x:  0, y: -1 } (Move Left  0)
      testReturnsOriginIfMoveOfDistanceZero { x: -1, y: -1 } (Move Left  0)

      let testTranslatesOriginIfMove = (\origin step expectedPosition ->
        it (
          "translates origin if step is a Move with distance ≠ 0"
          <> " (step: " <> show step <> ", origin: " <> show origin
          <> ", expectedPosition: " <> show expectedPosition <> ")"
        ) $ buildTestCase origin step expectedPosition
      )

      testTranslatesOriginIfMove { x:  0, y:  0 }  (Move Up      1)  { x:  0, y:  1 }
      testTranslatesOriginIfMove { x:  0, y:  0 }  (Move Up   (-1))  { x:  0, y: -1 }
      testTranslatesOriginIfMove { x:  0, y:  0 }  (Move Up      2)  { x:  0, y:  2 }
      testTranslatesOriginIfMove { x:  1, y:  2 }  (Move Up      1)  { x:  1, y:  3 }
      testTranslatesOriginIfMove { x:  0, y:  0 }  (Move Right    1)  { x:  1, y:  0 }
      testTranslatesOriginIfMove { x:  0, y:  0 }  (Move Right (-1))  { x: -1, y:  0 }
      testTranslatesOriginIfMove { x:  0, y:  0 }  (Move Right    2)  { x:  2, y:  0 }
      testTranslatesOriginIfMove { x:  1, y:  2 }  (Move Right    1)  { x:  2, y:  2 }
      testTranslatesOriginIfMove { x:  0, y:  0 }  (Move Down     1)  { x:  0, y: -1 }
      testTranslatesOriginIfMove { x:  0, y:  0 }  (Move Down  (-1))  { x:  0, y:  1 }
      testTranslatesOriginIfMove { x:  0, y:  0 }  (Move Down     2)  { x:  0, y: -2 }
      testTranslatesOriginIfMove { x:  1, y:  2 }  (Move Down     1)  { x:  1, y:  1 }
      testTranslatesOriginIfMove { x:  0, y:  0 }  (Move Left     1)  { x: -1, y:  0 }
      testTranslatesOriginIfMove { x:  0, y:  0 }  (Move Left  (-1))  { x:  1, y:  0 }
      testTranslatesOriginIfMove { x:  0, y:  0 }  (Move Left     2)  { x: -2, y:  0 }
      testTranslatesOriginIfMove { x:  1, y:  2 }  (Move Left     1)  { x:  0, y:  2 }

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
