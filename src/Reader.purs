module Reader where

import Control.Monad.Reader.Class (ask, asks, local)
import Control.Monad.Reader (Reader, runReader)
import Data.Lens (Lens', set, view)
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude


type Greeting = String
type Name = String
type Environment = { name :: String, greeting :: Greeting }

buildGreeting :: Greeting -> Name -> String
buildGreeting g n = g <> ", " <> n <> "!"


-- Readers are executed using `runReader`
main :: Effect Unit
main = do
  let env = { name: "mechpig", greeting: "hello" }
  log $ "greet:\n\t" <> runReader greet env
  log $ "greetAgain:\n\t" <> runReader greetAgain env
  log $ "greetProperly:\n\t" <> runReader greetProperly env
  log $ "greetAlsoGeeno:\n\t" <> runReader greetAlsoGeeno env


-- This computation uses `ask` to retrieve the environment
greet :: Reader Environment String
greet = do
  { name, greeting } <- ask
  pure $ buildGreeting greeting name

-- We can use `asks` to apply a function to the environment before it's returned
-- In this case we use getters to ask for single environment properties
greetAgain :: Reader Environment String
greetAgain = do
  name <- asks getName
  greeting <- asks getGreeting
  pure $ buildGreeting greeting name

-- `local` is used to modify the environment...
greetProperly :: Reader Environment String
greetProperly = do
  { name, greeting } <- local (setGreeting "good morning") ask
  pure $ buildGreeting greeting name

-- ...but only for a single computation
greetAlsoGeeno :: Reader Environment String
greetAlsoGeeno = do
  geeno <- local (setName "geeno") (asks getName)
  { name, greeting } <- ask
  pure $ (buildGreeting greeting name) <> " " <> (buildGreeting greeting geeno)


-- Some lenses to help accessing and manipulating the environment

_name :: forall a r. Lens' { name :: a | r } a
_name = prop (SProxy :: SProxy "name")

getName :: forall a r. { name :: a | r } -> a
getName = view _name

setName :: forall a r. a -> { name :: a | r } -> { name :: a | r }
setName = set _name

_greeting :: forall a r. Lens' { greeting :: a | r } a
_greeting = prop (SProxy :: SProxy "greeting")

getGreeting :: forall a r. { greeting :: a | r } -> a
getGreeting = view _greeting

setGreeting :: forall a r. a -> { greeting :: a | r } -> { greeting :: a | r }
setGreeting = set _greeting
