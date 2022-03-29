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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (NominalDiffTime, UTCTime)
import qualified Data.Time.Clock as Time
import Duration (Duration)
import qualified Duration
import NonEmptyText (NonEmptyText)
import qualified NonEmptyText
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

program :: Members '[Input Text, Output String, ActivityRepository, Random, Input UTCTime] r => Sem r ()
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

collectAction :: Members '[Input Text, Output String] r => Sem r Action
collectAction = do
  Output.output "Which of the following actions do you want to take?"
  Output.output "create (c) = Create a new Activity"
  Output.output "measure (m) = Measure an existing Activity"
  Output.output "predict (p) = Predict the next Measurement of an existing Activity"
  Output.output "quit (q) = Quit the program"
  Input.input >>= (actionFromText >>> maybe doAgain pure)
  where
    doAgain = do
      Output.output "I'm sorry! I did not understand that."
      Output.output "Let me try again."
      outputEmptyLine
      collectAction

createActivity :: Members '[Input Text, Output String, ActivityRepository, Random, Input UTCTime] r => Sem r ()
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

collectName :: Members '[Input Text, Output String] r => Sem r NonEmptyText
collectName = do
  Output.output "Provide Activity Name:"
  nameText <- Input.input
  case NonEmptyText.create nameText of
    Left errorMsg -> do
      outputEmptyLine
      Output.output (show errorMsg)
      outputEmptyLine
      collectName
    Right name -> return name

collectDuration :: Members '[Input Text, Output String] r => Sem r Duration
collectDuration = do
  Output.output "Provide Activity Duration:"
  Input.input >>= (T.unpack >>> Read.readEither >>> either doAgain mapDuration)
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
  Sem (Input Text : Output String : ActivityRepository : Random : Input UTCTime : r) a ->
  Sem r ()
consumeStoreRandomAndInputIO activityMap =
  Input.runInputSem (Polysemy.embed T.getLine)
    >>> Output.runOutputSem (putStrLn >>> Polysemy.embed)
    >>> Repository.runActivityRepositoryAsState
    >>> State.stateToIO activityMap
    >>> Random.runRandomIO
    >>> Input.runInputSem (Polysemy.embed Time.getCurrentTime)
    >>> Monad.void

actionFromText :: Text -> Maybe Action
actionFromText text =
  case T.unpack text of
    "create" -> Just CreateNewActivity
    "c" -> Just CreateNewActivity
    "measure" -> Just MeasureActivity
    "m" -> Just MeasureActivity
    "predict" -> Just PredictDuration
    "p" -> Just PredictDuration
    "quit" -> Just Quit
    "q" -> Just Quit
    _ -> Nothing

outputEmptyLine :: Member (Output String) r => Sem r ()
outputEmptyLine = Output.output ""
