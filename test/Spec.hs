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
          [ PluginSummary {pluginInfo = PluginInfo {piName = "maven-clean-plugin", piVersion = "2.5", piGoal = "clean"}, execution = "default-clean", artifactId = "lienzo-core", duration = 0}
          , PluginSummary {pluginInfo = PluginInfo {piName = "maven-resources-plugin", piVersion = "2.6", piGoal = "resources"}, execution = "default-resources", artifactId = "lienzo-core", duration = 1}
          , PluginSummary {pluginInfo = PluginInfo {piName = "maven-compiler-plugin", piVersion = "3.2", piGoal = "compile"}, execution = "default-compile", artifactId = "lienzo-core", duration = 4}
          , PluginSummary {pluginInfo = PluginInfo {piName = "maven-resources-plugin", piVersion = "2.6", piGoal = "testResources"}, execution = "default-testResources", artifactId = "lienzo-core", duration = 0}
          , PluginSummary {pluginInfo = PluginInfo {piName = "maven-compiler-plugin", piVersion = "3.2", piGoal = "testCompile"}, execution = "default-testCompile", artifactId = "lienzo-core", duration = 0}
          , PluginSummary {pluginInfo = PluginInfo {piName = "maven-surefire-plugin", piVersion = "2.12.4", piGoal = "test"}, execution = "default-test", artifactId = "lienzo-core", duration = 5}
          , PluginSummary {pluginInfo = PluginInfo {piName = "maven-jar-plugin", piVersion = "2.5", piGoal = "jar"}, execution = "default-jar", artifactId = "lienzo-core", duration = 1}
          , PluginSummary {pluginInfo = PluginInfo {piName = "maven-source-plugin", piVersion = "3.0.1", piGoal = "jar"}, execution = "attach-sources", artifactId = "lienzo-core", duration = 0}
          , PluginSummary {pluginInfo = PluginInfo {piName = "maven-install-plugin", piVersion = "2.4", piGoal = "install"}, execution = "default-install", artifactId = "lienzo-core", duration = 0}
          ]

  describe "Conext.parsePluginLine" $ do
    it "correctly parses plugin" $ do
      parsePluginLine "[INFO] --- tycho-source-plugin:1.0.0:plugin-source (default) @ drools-eclipse ---"
        `shouldBe` (PluginInfo "tycho-source-plugin" "1.0.0" "plugin-source", "default", "drools-eclipse")

    it "throws error on invalid input" $ do
      evaluate (parsePluginLine "")
        `shouldThrow` errorCall "Failed to parse plugin line '', error was 'not enough input'"
