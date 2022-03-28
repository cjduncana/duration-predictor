module Wizard (wizard) where

import ActivityAggregate (ActivityAggregate, ActivityId)
import ActivityAggregate.Repository (ActivityRepository)
import qualified ActivityAggregate.Repository as Repository
import ActivityAggregate.Repository.State (ActivityMap)
import qualified ActivityAggregate.Repository.State as Repository
import Control.Category ((>>>))
import qualified Control.Monad as Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (NominalDiffTime, UTCTime)
import qualified Data.Time.Clock as Time
import Duration (Duration)
import qualified Duration
import NonEmptyString (NonEmptyString)
import qualified NonEmptyString
import Polysemy (Embed, Member, Members, Sem)
import qualified Polysemy
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input
import Polysemy.Output (Output)
import qualified Polysemy.Output as Output
import Polysemy.Random (Random)
import qualified Polysemy.Random as Random
import qualified Polysemy.State as State
import qualified Text.Read as Read

data Action
  = CreateNewActivity
  | MeasureActivity
  | PredictDuration
  | Quit

wizard :: IO ()
wizard = Polysemy.runM (consumeStoreRandomAndInputIO Map.empty program)

-- Helper functions

program :: Members '[Input String, Output String, ActivityRepository, Random, Input UTCTime] r => Sem r ()
program = do
  outputEmptyLine
  action <- collectAction
  outputEmptyLine
  case action of
    CreateNewActivity -> createActivity >> program
    -- TODO
    MeasureActivity -> program
    -- TODO
    PredictDuration -> program
    Quit -> return ()

collectAction :: Members '[Input String, Output String] r => Sem r Action
collectAction = do
  Output.output "Which of the following actions do you want to take?"
  Output.output "create (c) = Create a new Activity"
  Output.output "measure (m) = Measure an existing Activity"
  Output.output "predict (p) = Predict the next Measurement of an existing Activity"
  Output.output "quit (q) = Quit the program"
  Input.input >>= (actionFromString >>> maybe doAgain pure)
  where
    doAgain = do
      Output.output "I'm sorry! I did not understand that."
      Output.output "Let me try again."
      outputEmptyLine
      collectAction

createActivity :: Members '[Input String, Output String, ActivityRepository, Random, Input UTCTime] r => Sem r ()
createActivity = do
  Output.output "Let's create a new Activity!"
  outputEmptyLine
  name <- collectName
  outputEmptyLine
  duration <- collectDuration
  outputEmptyLine
  activity <- Repository.create name duration
  Output.output $ "You created a new Activiy named '" ++ show activity ++ "'"
  outputEmptyLine

collectName :: Members '[Input String, Output String] r => Sem r NonEmptyString
collectName = do
  Output.output "Provide Activity Name:"
  nameString <- Input.input
  case NonEmptyString.create nameString of
    Left errorMsg -> do
      outputEmptyLine
      Output.output (show errorMsg)
      outputEmptyLine
      collectName
    Right name -> return name

collectDuration :: Members '[Input String, Output String] r => Sem r Duration
collectDuration = do
  Output.output "Provide Activity Duration:"
  Input.input >>= (Read.readEither >>> either doAgain mapDuration)
  where
    doAgain errorMsg = do
      outputEmptyLine
      Output.output (show errorMsg)
      outputEmptyLine
      collectDuration

    mapDuration = Duration.create >>> either doAgain return

consumeStoreRandomAndInputIO ::
  Member (Embed IO) r =>
  ActivityMap ->
  Sem (Input String : Output String : ActivityRepository : Random : Input UTCTime : r) a ->
  Sem r ()
consumeStoreRandomAndInputIO activityMap =
  Input.runInputSem (Polysemy.embed getLine)
    >>> Output.runOutputSem (putStrLn >>> Polysemy.embed)
    >>> Repository.runActivityRepositoryAsState
    >>> State.stateToIO activityMap
    >>> Random.runRandomIO
    >>> Input.runInputSem (Polysemy.embed Time.getCurrentTime)
    >>> Monad.void

actionFromString :: String -> Maybe Action
actionFromString "create" = Just CreateNewActivity
actionFromString "c" = Just CreateNewActivity
actionFromString "measure" = Just MeasureActivity
actionFromString "m" = Just MeasureActivity
actionFromString "predict" = Just PredictDuration
actionFromString "p" = Just PredictDuration
actionFromString "quit" = Just Quit
actionFromString "q" = Just Quit
actionFromString _ = Nothing

outputEmptyLine :: Member (Output String) r => Sem r ()
outputEmptyLine = Output.output ""
