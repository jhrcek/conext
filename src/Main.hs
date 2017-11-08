{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Control.Monad
import Data.Text (Text)
import Data.Text.IO as T
import Text.HTML.TagSoup (Tag, fromTagText, isTagText, parseTags, partitions)
import Text.HTML.TagSoup.Match (tagOpenAttrLit)

main :: IO ()
main = do
    input <- T.readFile "consoleFull.html"
    let tags = parseTags input
        timestampTextPairs = map (map fromTagText . filter isTagText) $ partitions isTimedLine tags
    mapM_ print timestampTextPairs

isTimedLine :: Tag Text -> Bool
isTimedLine =  tagOpenAttrLit "span" ("class", "timestamp")
