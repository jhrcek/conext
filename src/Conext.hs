{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Conext where

import           Control.Arrow           (first)
import           Control.Monad           (forM_, when)
import           Data.Attoparsec.Text    (Parser, char, parseOnly, string,
                                          takeWhile)
import           Data.Function           (on)
import           Data.List               (groupBy, sortBy)
import qualified Data.Map.Strict         as Map
import           Data.Monoid             ((<>))
import           Data.Ord                (comparing)
import qualified Data.Set                as Set
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Text.IO            as T
import           Data.Time.Clock         (DiffTime)
import           Data.Time.Format        (defaultTimeLocale, formatTime,
                                          parseTimeM)
import           Data.Time.LocalTime     (timeOfDayToTime, timeToTimeOfDay)
import           Prelude                 hiding (takeWhile)
import           System.Directory        (doesFileExist)
import           System.Environment      (getArgs)
import           System.Exit             (die)
import           Text.HTML.TagSoup       (Tag, fromTagText, isTagText,
                                          parseTags, partitions)
import           Text.HTML.TagSoup.Match (tagOpenAttrLit)

main :: IO ()
main = do
    infile <- parseArgs
    input <- T.readFile infile
    let pluginSummaries = parsePluginSummaries input

        artifact_plugin_durations = Text.unlines . ("artifact,plugin,duration":)
                . fmap (\(art, plug, dur) -> Text.intercalate "," [art, pluginInfoToText plug, formatDuration "%M:%S" dur])
                $ pluginAndArtifactBuildDurations pluginSummaries

        artifact_durations = Text.unlines . ("artifact,duration":)
                . fmap (\(art,  dur) -> Text.intercalate "," [art, formatDuration "%M:%S" dur])
                $ aftifactBuildDurations pluginSummaries

        plugin_durations = Text.unlines . ("plugin,duration":)
                . fmap (\(plug,  dur) -> Text.intercalate "," [plug, formatDuration "%X" dur])
                $ pluginBuildDurations pluginSummaries

        plugins_with_multiple_versions = pluginsWithMultipleVersions pluginSummaries

        output =
            [ ("artifact_plugin_durations.csv", artifact_plugin_durations)
            , ("artifact_durations.csv", artifact_durations)
            , ("plugin_durations.csv", plugin_durations)
            ]

    when (null pluginSummaries) $
        die $ "Failed to extract plugin duration info from " <> infile <>
              ". Was it really 'consoleFull' of a Jenkins job?"

    when (not $ null plugins_with_multiple_versions) $ do
        T.putStrLn "WARNING: multiple versions of some plugins used in the build process"
        T.putStrLn . Text.unlines $ fmap (\(plug, versions) -> plug <> " [" <> Text.intercalate ", " versions <> "]") plugins_with_multiple_versions

    forM_ output $ \(filename, content) -> do
        T.writeFile filename content
        T.putStrLn $ "Output written to file " <> Text.pack filename

formatDuration :: String -> DiffTime -> Text
formatDuration format d =
    Text.pack $ formatTime defaultTimeLocale format (timeToTimeOfDay d)

pluginAndArtifactBuildDurations :: [PluginSummary] -> [(Text, PluginInfo, DiffTime)]
pluginAndArtifactBuildDurations =
    map (\ps -> (artifactId ps, pluginInfo ps, duration ps))
    . sortBy (flip (comparing duration))

aftifactBuildDurations :: [PluginSummary] -> [(Text, DiffTime)]
aftifactBuildDurations =
    aggregateSummariesBy artifactId

pluginBuildDurations :: [PluginSummary] -> [(Text, DiffTime)]
pluginBuildDurations =
    map (first pluginInfoToText) . aggregateSummariesBy pluginInfo

pluginsWithMultipleVersions :: [PluginSummary] -> [(Text, [Text])]
pluginsWithMultipleVersions =
    Map.toList
    . fmap Set.toList
    . Map.filter (\versions -> Set.size versions > 1)
    . Map.fromListWith Set.union
    . fmap (\PluginSummary{pluginInfo=PluginInfo{piName,piVersion}} -> (piName, Set.singleton piVersion))

aggregateSummariesBy :: Ord a => (PluginSummary -> a) -> [PluginSummary] -> [(a, DiffTime)]
aggregateSummariesBy field = sortBy (flip (comparing snd))
    . map (\ps -> (field $ head ps , sum $ map duration ps))
    . groupBy ((==)`on`field)
    . sortBy (comparing field)

parsePluginSummaries :: Text -> [PluginSummary]
parsePluginSummaries =
    map entriesToSummary . partitionByPlugin . extractLogData

isTimedLine :: Tag Text -> Bool
isTimedLine =
    tagOpenAttrLit "span" ("class", "timestamp")

data LogEntry = LogEntry DiffTime Text deriving Show

type LogEntries = [LogEntry]

data PluginSummary = PluginSummary
    { pluginInfo :: !PluginInfo
    , execution  :: !Text
    , artifactId :: !Text
    , duration   :: !DiffTime
    , linesCount :: !Int
    } deriving (Eq, Show)

entriesToSummary :: LogEntries -> PluginSummary
entriesToSummary entries = PluginSummary{..}
  where
    (LogEntry start pluginLine) = head entries
    (LogEntry end _    ) = last entries
    duration = end - start
    (pluginInfo, execution, artifactId) = parsePluginLine pluginLine
    linesCount = length entries

-- Break "[INFO] --- maven-resources-plugin:3.0.2:resources (default-resources) @ kjar-with-instrumentation" into
-- into  (PluginInfo "maven-resources-plugin" "3.0.2" "resources", "default-resources", "kjar-with-instrumentation")
parsePluginLine :: Text -> (PluginInfo, Text, Text)
parsePluginLine pluginLine = either parseError id $ parseOnly pluginLineParser pluginLine
  where
    parseError e = textError $ "Failed to parse plugin line '" <> pluginLine <> "', error was '" <> Text.pack e <> "'"

pluginLineParser :: Parser (PluginInfo, Text, Text)
pluginLineParser = (,,)
    <$> (string "[INFO] --- " *> pluginInfoParser)
    <*> (string " ("          *> takeWhile (/=')'))
    <*> (string ") @ "        *> takeWhile (/=' '))
    <* string " ---"

pluginInfoParser :: Parser PluginInfo
pluginInfoParser = PluginInfo
    <$> (takeWhile (/=':') <* char (':'))
    <*> (takeWhile (/=':') <* char (':'))
    <*> takeWhile (/=' ')

data PluginInfo = PluginInfo
    { piName    :: !Text
    , piVersion :: !Text
    , piGoal    :: !Text
    } deriving (Eq, Ord, Show)

pluginInfoToText :: PluginInfo -> Text
pluginInfoToText PluginInfo{..} =
    Text.intercalate ":" [piName, piVersion, piGoal]

pluginSummaryToText :: PluginSummary -> Text
pluginSummaryToText PluginSummary{..} =
    Text.concat [pluginInfoToText pluginInfo, " (", execution, ") @ ", artifactId]

partitionByPlugin :: LogEntries -> [LogEntries]
partitionByPlugin =
    partitions (\(LogEntry _ txt)-> Text.isPrefixOf "[INFO] --- " txt && Text.isInfixOf " @ " txt)

extractLogData :: Text -> [LogEntry]
extractLogData =
    map scrapedLineToLogEntry . partitions isTimedLine . parseTags

scrapedLineToLogEntry :: [Tag Text] -> LogEntry
scrapedLineToLogEntry =
  (\(timestamp:rest) -> LogEntry (parseTimestamp timestamp) (Text.strip $ Text.concat rest))
  . map fromTagText
  . filter isTagText

parseTimestamp :: Text -> DiffTime
parseTimestamp ts =
  case parseTimeM False defaultTimeLocale "%H:%M:%S" (Text.unpack ts) of
    Just tod -> timeOfDayToTime tod
    Nothing  -> textError $ "Failed to parse timestamp " <> ts

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
dieOnInvalidInput =
    die "Please supply file containing consoleFull text of jenkins job to analyze"

textError :: Text -> a
textError =
    error . Text.unpack
