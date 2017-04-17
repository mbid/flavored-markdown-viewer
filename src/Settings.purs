module Settings
( Settings
, SettingsOverride
, emptyOverride
, override
, minimize
, defaultSettings
) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Seconds(..))
import GithubMarkdown (GithubAuth)

type Settings =
  { githubAuth :: Maybe GithubAuth
  , reloadInterval :: Seconds
  }
type SettingsOverride =
  { githubAuth :: Maybe (Maybe GithubAuth)
  , reloadInterval :: Maybe Seconds
  }

defaultSettings :: Settings
defaultSettings =
  { githubAuth: Nothing
  , reloadInterval: Seconds 0.5
  }

emptyOverride :: SettingsOverride
emptyOverride =
  { githubAuth: Nothing
  , reloadInterval: Nothing
  }

override :: SettingsOverride -> Settings -> Settings
override o s =
  { githubAuth: fromMaybe s.githubAuth o.githubAuth
  , reloadInterval: fromMaybe s.reloadInterval o.reloadInterval
  }

minimizeOne :: forall a. Eq a => a -> Maybe a -> Maybe a
minimizeOne x mx = if Just x == mx then Nothing else mx

minimize :: Settings -> SettingsOverride -> SettingsOverride
minimize s o =
  { githubAuth: minimizeOne s.githubAuth o.githubAuth
  , reloadInterval: minimizeOne s.reloadInterval o.reloadInterval
  }
