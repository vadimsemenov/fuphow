module Comonads.ProjectBuilder
       ( Project
       , projectName
       , hasBenchs
       , hasGithub
       , hasTravis
       , ProjectBuilder
       , buildProject
       , defaultSettings
       , benchs
       , github
       , travis
       ) where


import           Control.Comonad.Traced (Traced, runTraced, traced)
import           Data.Text              (Text)


data ProjectSettings = ProjectSettings
     { settingsBenchs :: Bool  -- ^ enable benchmarks for project?
     , settingsGithub :: Bool  -- ^ set up github     for project?
     , settingsTravis :: Bool  -- ^ set up Travis CI  for project?
     }

instance Monoid ProjectSettings where
    mempty = defaultSettings
    lhs `mappend` rhs = ProjectSettings (settingsBenchs lhs || settingsBenchs rhs)
                                        (settingsGithub lhs || settingsGithub rhs)
                                        (settingsTravis lhs || settingsTravis rhs)

data Project = Project
     { projectName :: Text
     , hasBenchs   :: Bool
     , hasGithub   :: Bool
     , hasTravis   :: Bool
     } deriving (Show)

type ProjectBuilder = Traced ProjectSettings Project -- ProjectSettings -> Project


buildProject :: Text -> ProjectBuilder
buildProject name = traced $ \settings -> Project name
                                                  (settingsBenchs settings)
                                                  (settingsGithub settings)
                                                  (settingsTravis settings)

defaultSettings :: ProjectSettings
defaultSettings = ProjectSettings False False False

benchs :: ProjectBuilder -> Project
benchs builder = runTraced builder $ defaultSettings { settingsBenchs = True }

github :: ProjectBuilder -> Project
github builder = runTraced builder $ defaultSettings { settingsGithub = True }

travis :: ProjectBuilder -> Project
travis builder = let proj = runTraced builder defaultSettings in
    if hasGithub proj
    then runTraced builder $ defaultSettings { settingsTravis = True }
    else proj

{-
instance Monoid m => Comonad ((->)m) where
  duplicate f m = f . mappend m
  extract f = f mempty

wa =>> f ≡ extend f wa ≡ fmap f (duplicate wa)
≡ fmap f (\m -> wa . mappend m)
≡ f . (\m -> wa . mappend m)
≡ f . (\m m1 -> wa (m <> m1))
≡ \m2 -> f ((\m m1 -> wa (m <> m1)) m2)
≡ \m2 -> f (\m1 -> wa (m2 <> m1))

wa =>> f =>> g
≡ (\m2 -> f (\m1 -> wa (m2 <> m1))) =>> g
≡ \s -> g (\m2 -> f (\m1 -> wa (s <> m2 <> m1)))

builder =>> travis =>> github
≡ \s -> github (\m2 -> travis (\m1 -> builder (s <> m2 <> m1)))
≡ \s -> (\m2 -> travis (\m1 -> builder (s <> m2 <> m1))) $ defaultSettings { settingsGithub = True }
≡ \s -> (travis (\m1 -> builder (s <> defaultSettings { settingsGithub = True } <> m1)))
≡ \s -> (travis (\m1 -> builder (s <> m1 <> defaultSettings { settingsGithub = True })))
≡ \s -> (travis (\m1 -> (\m2 -> builder (s <> m1 <> m2) $ defaultSettings { settingsGithub = True })))
≡ \s -> (travis (\m1 -> github (\m2 -> builder (s <> m1 <> m2))))
≡ builder =>> github =>> travis
-- -}
