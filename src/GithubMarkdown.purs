module GithubMarkdown where

import Prelude
import Data.Argonaut as A
import Data.StrMap as StrMap
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (Error)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(POST))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX, defaultRequest, affjax, AffjaxResponse, AffjaxRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.StatusCode (StatusCode(..))
import Text.Base64 (encode64)


newtype GithubAuth = GithubAuth { username :: String, password :: String }

username (GithubAuth auth) = auth.username
password (GithubAuth auth) = auth.password

derive instance eqGithubAuth :: Eq GithubAuth

data GithubApiError = RateLimited
                    | UnknownError String
                    | AffjaxError Error

instance showGithubApiError :: Show GithubApiError where
  show RateLimited = "RateLimited"
  show (UnknownError err) = "UnknownError " <> err
  show (AffjaxError err) = "AffjaxError " <> show err

showResponse :: forall a. Show a => AffjaxResponse a -> String
showResponse resp = 
  "{ status: " <> show resp.status <>
  ", headers: " <> show resp.headers <>
  ", response: " <> show resp.response <>
  "}"

markdownToHtml :: forall e.
  Maybe GithubAuth ->
  String ->
  Aff (ajax :: AJAX | e) (Either GithubApiError String)
markdownToHtml auth src = do
  resp <- attempt $ affjax req
  pure $ case resp of
    Left err -> Left $ AffjaxError err
    Right resp'
      | resp'.status == StatusCode 200 -> Right resp'.response
      | resp'.status == StatusCode 403 -> Left RateLimited
      | otherwise -> Left $ UnknownError $ showResponse resp'
  where
    content :: String
    content =
      A.printJson $ A.fromObject $ StrMap.fromFoldable
      [ Tuple "text" $ A.fromString src
      , Tuple "mode" $ A.fromString "markdown" ]
    
    authHeader :: Maybe RequestHeader
    authHeader = case auth of
      Nothing -> Nothing
      Just (GithubAuth auth') ->
        Just $
        RequestHeader "Authorization" $
        "Basic " <> (encode64 $ auth'.username <> ":" <> auth'.password)
    req :: AffjaxRequest String
    req =
      defaultRequest
      { url = "https://api.github.com/markdown"
      , method = Left POST
      , content = Just content
      , username = username <$> auth
      , password = password <$> auth
      , headers = defaultRequest.headers <> fromFoldable authHeader
      }
