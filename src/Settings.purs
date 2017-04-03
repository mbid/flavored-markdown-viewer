module Settings (Settings, SettingsOverride, emptyOverride, override) where

import Data.Maybe (Maybe(..), fromMaybe)
import GithubMarkdown (GithubAuth)

type Settings = { auth :: Maybe GithubAuth }
type SettingsOverride = { auth :: Maybe (Maybe GithubAuth) }

emptyOverride :: SettingsOverride
emptyOverride =
  { auth: Nothing
  }

override :: SettingsOverride -> Settings -> Settings
override o s =
  { auth: fromMaybe s.auth o.auth
  }
