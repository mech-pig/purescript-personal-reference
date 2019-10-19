module Test.Reader.Spec where

import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Reader (buildGreeting, greet, greetAgain, greetProperly, greetAlsoGeeno)
import Control.Monad.Reader (runReader)


spec :: Spec Unit
spec =
  describe "Test.Reader" do

    describe "buildGreeting" do
      it "returns a formatted greeting" do
        (buildGreeting "Hi" "Geeno") `shouldEqual` "Hi, Geeno!"
        (buildGreeting "hello" "world") `shouldEqual` "hello, world!"

    describe "greet" do
      it "returns a greeting built from the given environment" $
        runReader greet { name: "world", greeting: "hello" } `shouldEqual` "hello, world!"

    describe "greetAgain" do
      it "returns the same result returned by `greet`" $
        runReader greetAgain { name: "world", greeting: "hello" } `shouldEqual` runReader greet { name: "world", greeting: "hello" }

    describe "greetProperly" do
      it "returns a more formal greeting" $
        runReader greetProperly { name: "world", greeting: "hello" } `shouldEqual` "good morning, world!"

    describe "greetAlsoGeeno" do
      it "doesn't forget to greet Geeno" $
        runReader greetAlsoGeeno { name: "world", greeting: "hello" } `shouldEqual` "hello, world! hello, geeno!"
