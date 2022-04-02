module Wizard (wizard) where

import ActivityAggregate (ActivityAggregate, ActivityId)
import ActivityAggregate.Repository (ActivityRepository, RepositoryError)
import qualified ActivityAggregate.Repository as Repository
import ActivityAggregate.Repository.State (ActivityMap)
import qualified ActivityAggregate.Repository.State as Repository
import Control.Category ((>>>))
import qualified Control.Monad as Monad
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (NominalDiffTime, UTCTime)
import qualified Data.Time.Clock as Time
import Duration (Duration)
import qualified Duration
import qualified Entity
import qualified Indexable
import NonEmptyText (NonEmptyText)
import qualified NonEmptyText
import Polysemy (Embed, Member, Members, Sem)
import qualified Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
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
wizard = consumeProgramIO Map.empty program

-- Helper functions

program ::
  Members
    '[ Input Text,
       Output String,
       ActivityRepository,
       Random,
       Input UTCTime,
       Error RepositoryError
     ]
    r =>
  Sem r ()
program = do
  mActivities <- Repository.listActivities <&> NonEmpty.nonEmpty
  case mActivities of
    Nothing -> askWhenEmpty
    Just activities -> do
      outputEmptyLine
      action <- collectAction
      outputEmptyLine
      case action of
        CreateNewActivity -> createActivity >> program
        MeasureActivity -> measureActivity activities >> program
        PredictDuration -> predictDuration activities >> program
        Quit -> outputGoodbye

askWhenEmpty ::
  Members
    '[ Input Text,
       Output String,
       ActivityRepository,
       Random,
       Input UTCTime,
       Error RepositoryError
     ]
    r =>
  Sem r ()
askWhenEmpty = do
  Output.output "There are no Activities recorded."
  outputEmptyLine
  Output.output "Do you want to create a new Activity?"
  answer <- Input.inputs (T.toLower >>> T.head)
  case answer of
    'y' -> createActivity >> program
    'n' -> outputGoodbye
    _ -> do
      outputMisunderstanding
      askWhenEmpty

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
      outputMisunderstanding
      collectAction

createActivity ::
  Members
    '[ Input Text,
       Output String,
       ActivityRepository,
       Random,
       Input UTCTime
     ]
    r =>
  Sem r ()
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

measureActivity ::
  Members
    '[ Input Text,
       Output String,
       ActivityRepository,
       Random,
       Input UTCTime,
       Error RepositoryError
     ]
    r =>
  NonEmpty ActivityAggregate ->
  Sem r ()
measureActivity activities = do
  Output.output "Let's add a new Measurement!"
  outputEmptyLine
  displayActivities activities
  outputEmptyLine
  Output.output "Which Activity do you want to measure?"
  selectActivity activities measureActivity whenActivitySelected
  where
    whenActivitySelected activity = do
      duration <- collectDuration
      newActivity <- Repository.measure (Entity.getId activity) duration
      Output.output $ "You measured '" ++ show newActivity ++ "' as lasting " ++ show duration ++ " seconds."
      outputEmptyLine

predictDuration ::
  Members
    '[ Input Text,
       Output String,
       ActivityRepository,
       Error RepositoryError
     ]
    r =>
  NonEmpty ActivityAggregate ->
  Sem r ()
predictDuration activities = do
  Output.output "Let's predict the next Measurement!"
  outputEmptyLine
  displayActivities activities
  outputEmptyLine
  Output.output "Which Activity do you want to predict?"
  selectActivity activities predictDuration whenActivitySelected
  where
    whenActivitySelected activity = do
      duration <- Repository.predictDuration (Entity.getId activity)
      Output.output $ "We predict that '" ++ show activity ++ "' will last " ++ show duration ++ " seconds."
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

selectActivity ::
  Members '[Input Text, Output String] r =>
  NonEmpty ActivityAggregate ->
  (NonEmpty ActivityAggregate -> Sem r ()) ->
  (ActivityAggregate -> Sem r ()) ->
  Sem r ()
selectActivity activities whenActivitiesFailed whenActivitySelected = do
  eAnswer <- Input.inputs (T.unpack >>> Read.readEither)
  either mapError mapValidIndex eAnswer
  where
    mapError errorMsg = do
      outputEmptyLine
      Output.output errorMsg
      outputMisunderstanding
      outputEmptyLine
      whenActivitiesFailed activities

    mapValidIndex =
      (\i -> i - 1)
        >>> Indexable.index activities
        >>> either whenInvalidIndex whenActivitySelected

    whenInvalidIndex error = do
      outputEmptyLine
      Output.output $ show error
      Output.output "Let me try again."
      outputEmptyLine
      whenActivitiesFailed activities

displayActivities :: Member (Output String) r => NonEmpty ActivityAggregate -> Sem r ()
displayActivities =
  NonEmpty.zip infiniteIndex
    >>> Monad.mapM_ outputActivity
  where
    infiniteIndex = NonEmpty.iterate (1 +) 1
    outputActivity (index, activity) = Output.output (show index ++ ") " ++ show activity)

consumeProgramIO ::
  ActivityMap ->
  Sem
    '[ Input Text,
       Output String,
       ActivityRepository,
       Random,
       Input UTCTime,
       Error RepositoryError,
       Embed IO
     ]
    a ->
  IO ()
consumeProgramIO activityMap =
  Input.runInputSem (Polysemy.embed T.getLine)
    >>> Output.runOutputSem (putStrLn >>> Polysemy.embed)
    >>> Repository.runActivityRepositoryAsState
    >>> State.stateToIO activityMap
    >>> Random.runRandomIO
    >>> Input.runInputSem (Polysemy.embed Time.getCurrentTime)
    >>> Error.runError
    >>> Monad.void
    >>> Polysemy.runM

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

outputMisunderstanding :: Member (Output String) r => Sem r ()
outputMisunderstanding = do
  Output.output "I'm sorry! I did not understand that."
  Output.output "Let me try again."
  outputEmptyLine

outputGoodbye :: Member (Output String) r => Sem r ()
outputGoodbye = Output.output "Goodbye!"