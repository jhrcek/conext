{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char (isSpace)
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.IO as T
import Data.Time.Clock (DiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.LocalTime (timeOfDayToTime, timeToTimeOfDay)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (die)
import Text.HTML.TagSoup (Tag, fromTagText, isTagText, parseTags, partitions)
import Text.HTML.TagSoup.Match (tagOpenAttrLit)

main :: IO ()
main = do
    infile <- parseArgs
    input <- T.readFile infile
    let pluginSummaries = process input

        artifact_plugin_durations = Text.unlines . ("artifact,plugin,duration":)
                . fmap (\(art, plug, dur) -> Text.intercalate "," [art, plug, formatDuration "%M:%S" dur])
                $ pluginAndArtifactBuildDurations pluginSummaries

        artifact_durations = Text.unlines . ("artifact,duration":)
                . fmap (\(art,  dur) -> Text.intercalate "," [art, formatDuration "%M:%S" dur])
                $ aftifactBuildDurations pluginSummaries

        plugin_durations = Text.unlines . ("plugin,duration":)
                . fmap (\(plug,  dur) -> Text.intercalate "," [plug, formatDuration "%X" dur])
                $ pluginBuildDurations pluginSummaries

    T.writeFile "artifact_plugin_durations.csv" artifact_plugin_durations
    T.writeFile "artifact_durations.csv" artifact_durations
    T.writeFile "plugin_durations.csv" plugin_durations


formatDuration :: String -> DiffTime -> Text
formatDuration format d = Text.pack $ formatTime defaultTimeLocale format (timeToTimeOfDay d)


pluginAndArtifactBuildDurations :: [PluginSummary] -> [(Text, Text, DiffTime)]
pluginAndArtifactBuildDurations =
    map (\ps -> (artifactId ps, pluginName ps, duration ps)) {-takeWhile ((>10) . duration)-}
    . sortBy (flip (comparing duration))


aftifactBuildDurations :: [PluginSummary] -> [(Text, DiffTime)]
aftifactBuildDurations = aggregateSummariesBy artifactId


pluginBuildDurations :: [PluginSummary] -> [(Text, DiffTime)]
pluginBuildDurations = aggregateSummariesBy pluginName


aggregateSummariesBy :: (PluginSummary -> Text) -> [PluginSummary] -> [(Text, DiffTime)]
aggregateSummariesBy field = sortBy (flip (comparing snd))
    . map (\ps -> (field $ head ps , sum $ map duration ps))
    . groupBy ((==)`on`field)
    . sortBy (comparing field)


process :: Text -> [PluginSummary]
process = map entriesToSummary . partitionPlugins . extractLogData


isTimedLine :: Tag Text -> Bool
isTimedLine =  tagOpenAttrLit "span" ("class", "timestamp")


data LogEntry = LogEntry DiffTime Text deriving Show


type PluginEntries = [LogEntry]


data PluginSummary = PluginSummary
    { pluginName :: Text
    , execution  :: Text
    , artifactId :: Text
    , logText    :: Text
    , duration   :: DiffTime
    } deriving Show


entriesToSummary :: PluginEntries -> PluginSummary
entriesToSummary entries = PluginSummary
    { pluginName = pluginName'
    , execution = execution'
    , artifactId = artifactId'
    , logText = Text.unlines $ map getPayload entries
    , duration = end - start
    }
  where
    (LogEntry start txt) = head entries
    (LogEntry end _    ) = last entries
    -- Break "[INFO] --- maven-resources-plugin:3.0.2:resources (default-resources) @ kjar-with-instrumentation" into
    -- into            ("maven-resources-plugin:3.0.2:resources", "(default-resources)", "kjar-with-instrumentation")
    (pluginName', execution', artifactId') = case break (=="@") $ Text.words txt of
        (beforeAtSign, [_,art,_]) ->
            let [plugin, exec] = drop (length beforeAtSign - 2) beforeAtSign
            in (plugin, exec, art)
        _ -> error . Text.unpack $ "unexpected format " <> txt


getPayload :: LogEntry -> Text
getPayload (LogEntry _ payload) = payload


partitionPlugins :: [LogEntry] -> [PluginEntries]
partitionPlugins = partitions (\(LogEntry _ txt)-> Text.isInfixOf "---" txt && Text.isInfixOf " @ " txt)


extractLogData :: Text -> [LogEntry]
extractLogData = map scrapedLineToLogEntry . partitions isTimedLine . parseTags


scrapedLineToLogEntry :: [Tag Text] -> LogEntry
scrapedLineToLogEntry =
  (\(timestamp:rest) -> LogEntry (parseTimestamp timestamp) (trim $ Text.concat rest))
  . map fromTagText
  . filter isTagText


trim :: Text -> Text
trim = Text.dropWhile isSpace . Text.dropWhileEnd isSpace


parseTimestamp :: Text -> DiffTime
parseTimestamp ts =
  case parseTimeM False defaultTimeLocale "%H:%M:%S" (Text.unpack ts) of
    Just tod -> timeOfDayToTime tod
    Nothing  -> error $ "Failed to parse timestamp " ++ show ts


parseArgs :: IO FilePath
parseArgs = do
  args <- getArgs
  case args of
    [infile] -> do
       exists <- doesFileExist infile
       if exists then return infile
                 else dieOnInvalidInput
    _ -> dieOnInvalidInput


dieOnInvalidInput ::  IO a
dieOnInvalidInput = die "Please supply file containing consoleFull text of jenkins job to analyze"
