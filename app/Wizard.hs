module Wizard (wizard) where

import ActivityAggregate (ActivityAggregate)
import qualified ActivityAggregate
import qualified Data.Time.Clock as Time
import qualified Data.UUID.V4 as UUID
import Duration (Duration)
import qualified Duration
import NonEmptyString (NonEmptyString)
import qualified NonEmptyString
import qualified Text.Read as Read

wizard :: IO ()
wizard = do
  activityAggregate <- createActivityAgregate
  let prediction = ActivityAggregate.predictDuration activityAggregate
  putStrLn $ show activityAggregate ++ " will last " ++ show prediction

createActivityAgregate :: IO ActivityAggregate
createActivityAgregate = do
  name <- collectName
  duration <- collectDuration
  createActivityAgregate' name duration
    <$> UUID.nextRandom
    <*> UUID.nextRandom
    <*> Time.getCurrentTime
  where
    createActivityAgregate' name duration activityUuid measurementUuid =
      ActivityAggregate.create activityUuid name measurementUuid duration

-- Helper functions

collectName :: IO NonEmptyString
collectName = do
  putStrLn "Provide Activity Name:"
  nameString <- getLine
  either mapError return (NonEmptyString.create nameString)
  where
    mapError errorMsg = do
      print errorMsg
      collectName

collectDuration :: IO Duration
collectDuration = do
  putStrLn "Provide Activity Duration:"
  durationString <- getLine
  either mapError mapDuration (Read.readEither durationString)
  where
    mapError errorMsg = do
      print errorMsg
      collectDuration

    mapDuration duration =
      either mapError return (Duration.create duration)
