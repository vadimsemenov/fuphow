{-# LANGUAGE OverloadedStrings #-}
module Comonads.ProjectBuilderSpec
       ( spec
       ) where

import           Comonads.ProjectBuilder
import           Control.Comonad         (extract, (=>>))
import           Test.Hspec


spec :: Spec
spec =
    describe "Project Builder" $ do
        it "default" $
            extract (buildProject "cool-project") `shouldSatisfy` \p ->
                (not . hasBenchs) p && (not . hasGithub) p && (not . hasTravis) p
        it "with benchs" $
            extract (buildProject "cool-project" =>> benchs) `shouldSatisfy` \p ->
                hasBenchs p && (not . hasGithub) p && (not . hasTravis) p
        it "with github" $
            extract (buildProject "cool-project" =>> github) `shouldSatisfy` \p ->
                (not . hasBenchs) p && hasGithub p && (not . hasTravis) p
        it "travis without github" $
            extract (buildProject "cool-project" =>> travis) `shouldSatisfy` \p ->
                (not . hasBenchs) p && (not . hasGithub) p && (not . hasTravis) p
        it "with travis and github" $
            extract (buildProject "cool-project" =>> github =>> travis)
                `shouldSatisfy` \p ->
                (not . hasBenchs) p && hasGithub p && hasTravis p
        it "with travis and github (reverse order)" $
            extract (buildProject "cool-project" =>> travis =>> github)
                `shouldSatisfy` (\p ->
                (not . hasBenchs) p && hasGithub p && hasTravis p)
        it "all inclusive" $
            extract (buildProject "all" =>> travis =>> github =>> benchs)
                `shouldSatisfy` \p ->
                hasBenchs p && hasGithub p && hasTravis p
