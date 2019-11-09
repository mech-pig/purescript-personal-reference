module MonadTransformers where

import Prelude ((<>), ($), (+), (-), class Show, bind, discard, show, Unit)

import Control.Monad (class Monad)
import Control.Monad.Reader.Class (class MonadReader, asks)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.State.Class (class MonadState, modify)
import Control.Monad.State.Trans (evalStateT)
import Data.List (List(..), (:))
import Data.Foldable (foldl)
import Effect (Effect)
import Effect.Console (log)


-- Entities
data Step = Move Direction Int | Rest

data Direction = Up | Right | Left | Down

instance showStep :: Show Step where
  show (Rest) = "Rest"
  show (Move Up d) = "Move Up " <> show d
  show (Move Left d) = "Move Left " <> show d
  show (Move Down d) = "Move Down " <> show d
  show (Move Right d) = "Move Right " <> show d

type Instructions = List Step

type Point = { x :: Int, y :: Int }

type Path = List Point

move :: Step -> Point -> Point
move Rest origin = origin
move (Move Up d)    { x, y } = { x, y: y + d }
move (Move Right d) { x, y } = { x: x + d, y }
move (Move Down d)  { x, y } = { x, y: y - d }
move (Move Left d)  { x, y } = { x: x - d, y }


-- State
type RoverState = { currentPosition :: Point }  -- totalDistance :: Int

updateRoverState :: Step -> RoverState -> RoverState
updateRoverState step state = {
  currentPosition: move step state.currentPosition
}


-- Reader
type RoverConfiguration = { instructions :: Instructions }


roverProgram :: forall m.
                MonadReader RoverConfiguration m =>
                MonadState RoverState m =>
                m RoverState
roverProgram = do
  instructions <- asks _.instructions
  modify (\initState -> executeProgram initState instructions)
  where
    executeProgram = foldl (\currentState step -> updateRoverState step currentState)


runRoverProgram :: forall m. Monad m => RoverConfiguration -> RoverState -> m RoverState
runRoverProgram roverConfiguration initialState =
  evalStateT (runReaderT roverProgram roverConfiguration) initialState


main :: Effect Unit
main = do
  let initialState = { currentPosition: { x: 0, y: 1 } }
  let configuration = { instructions: ((Move Up 3) : (Move Left 1) : Nil) }
  log $ "initialState:\n\t" <> show initialState
  log $ "configuration:\n\t" <> show configuration
  finalState <- runRoverProgram configuration initialState
  log $ "finalState:\n\t" <> show finalState
