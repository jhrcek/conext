{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Data.Char (isSpace)
import Data.List (sortBy)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.IO as T
import Data.Time.Clock (DiffTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Time.LocalTime (timeOfDayToTime, timeToTimeOfDay)
import System.Environment (getArgs)
import Text.HTML.TagSoup (Tag, fromTagText, isTagText, parseTags, partitions)
import Text.HTML.TagSoup.Match (tagOpenAttrLit)

main :: IO ()
main = do
    [infile] <- getArgs
    input <- T.readFile infile
    let pluginSummaries = process input
    --mapM_ print pluginSummaries
    mapM_ print . map (\ps -> (artifactId ps , pluginName ps, timeToTimeOfDay $ duration ps)) . takeWhile ((>60) . duration) . reverse $ sortBy (comparing duration) pluginSummaries

process :: Text -> [PluginSummary]
process = map entriesToSummary . partitionPlugins . extractLogData

isTimedLine :: Tag Text -> Bool
isTimedLine =  tagOpenAttrLit "span" ("class", "timestamp")

data LogEntry = LogEntry DiffTime Text deriving Show

type PluginEntries = [LogEntry]

data PluginSummary = PluginSummary
    { pluginName ::Text
    , execution  :: Text
    , artifactId ::Text
    , logText    :: Text
    , duration   :: DiffTime
    } deriving Show

entriesToSummary :: PluginEntries -> PluginSummary
entriesToSummary entries = PluginSummary
    { pluginName = pluginName
    , execution = execution
    , artifactId = artifactId
    , logText = Text.unlines $ map getPayload entries
    , duration = end - start
    }
  where
    (LogEntry start txt) = head entries
    (LogEntry end _    ) = last entries
    -- Break "[INFO] --- maven-resources-plugin:3.0.2:resources (default-resources) @ kjar-with-instrumentation" into
    -- into            ("maven-resources-plugin:3.0.2:resources", "(default-resources)", "kjar-with-instrumentation")
    (pluginName, execution, artifactId) = case break (=="@") $ Text.words txt of
        (beforeAtSign, [_,art,_]) ->
            let [plugin, exec] = drop (length beforeAtSign - 2) beforeAtSign
            in (plugin, execution, art)
        xs -> error . Text.unpack $ "unexpected format " <> txt

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
