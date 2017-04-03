module GithubMarkdown where

import Prelude
import Network.HTTP.Affjax (AJAX, defaultRequest, affjax, AffjaxResponse, AffjaxRequest)
import Network.HTTP.StatusCode (StatusCode(..))
import Network.HTTP.RequestHeader (RequestHeader(..))
import Data.HTTP.Method (Method(POST))
import Data.Argonaut as A
import Data.Tuple (Tuple(..))
import Data.StrMap as StrMap
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Control.Monad.Aff (Aff)
import Text.Base64 (encode64)
import Data.Array (fromFoldable)


type GithubAuth = { username :: String, password :: String }

data GithubApiError = RateLimited
                    | UnknownError (AffjaxResponse String)

instance showGithubApiError :: Show GithubApiError where
  show RateLimited = "RateLimited"
  show (UnknownError resp) =
    "{ status: " <> show resp.status <>
    ", headers: " <> show resp.headers <>
    ", response: " <> show resp.response <>
    "}"

markdownToHtml :: forall e.
  Maybe GithubAuth ->
  String ->
  Aff (ajax :: AJAX | e) (Either GithubApiError String)
markdownToHtml auth src = do
  resp <- affjax req
  pure $ case resp.status of
    StatusCode 200 -> Right resp.response
    StatusCode 401 -> Left RateLimited
    c -> Left $ UnknownError $ resp
  where
    content :: String
    content =
      A.printJson $ A.fromObject $ StrMap.fromFoldable
      [ Tuple "text" $ A.fromString src
      , Tuple "mode" $ A.fromString "markdown" ]
    
    authHeader :: Maybe RequestHeader
    authHeader = case auth of
      Nothing -> Nothing
      Just auth' ->
        Just $
        RequestHeader "Authorization" $
        "Basic " <> (encode64 $ auth'.username <> ":" <> auth'.password)
    req :: AffjaxRequest String
    req =
      defaultRequest
      { url = "https://api.github.com/markdown"
      , method = Left POST
      , content = Just content
      , username = _.username <$> auth
      , password = _.password <$> auth
      , headers = defaultRequest.headers <> fromFoldable authHeader
      }
