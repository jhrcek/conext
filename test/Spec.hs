{-# LANGUAGE OverloadedStrings #-}
import           Test.Hspec

import           Conext            hiding (main)
import           Control.Exception (evaluate)
import           Data.Text.IO      as Text

main :: IO ()
main = hspec $ do
  describe "Conext.parsePluginSummaries" $ do
    it "correctly parses piece of log" $ do
      input <- Text.readFile "test/pieceOf_consoleFull"
      parsePluginSummaries input `shouldBe`
          [ PluginSummary (PluginInfo "maven-clean-plugin" "2.5" "clean") "default-clean" "lienzo-core" 0 2
          , PluginSummary (PluginInfo "maven-resources-plugin" "2.6" "resources") "default-resources" "lienzo-core" 1 5
          , PluginSummary (PluginInfo "maven-compiler-plugin" "3.2" "compile") "default-compile" "lienzo-core" 4 8
          , PluginSummary (PluginInfo "maven-resources-plugin" "2.6" "testResources") "default-testResources" "lienzo-core" 0 4
          , PluginSummary (PluginInfo "maven-compiler-plugin" "3.2" "testCompile") "default-testCompile" "lienzo-core" 0 4
          , PluginSummary (PluginInfo "maven-surefire-plugin" "2.12.4" "test") "default-test" "lienzo-core" 5 24
          , PluginSummary (PluginInfo "maven-jar-plugin" "2.5" "jar") "default-jar" "lienzo-core" 1 7
          , PluginSummary (PluginInfo "maven-source-plugin" "3.0.1" "jar") "attach-sources" "lienzo-core" 0 3
          , PluginSummary (PluginInfo "maven-install-plugin" "2.4" "install") "default-install" "lienzo-core" 0 11
          ]

  describe "Conext.parsePluginLine" $ do
    it "correctly parses plugin" $ do
      parsePluginLine "[INFO] --- tycho-source-plugin:1.0.0:plugin-source (default) @ drools-eclipse ---"
        `shouldBe` (PluginInfo "tycho-source-plugin" "1.0.0" "plugin-source", "default", "drools-eclipse")

    it "throws error on invalid input" $ do
      evaluate (parsePluginLine "")
        `shouldThrow` errorCall "Failed to parse plugin line '', error was 'not enough input'"
