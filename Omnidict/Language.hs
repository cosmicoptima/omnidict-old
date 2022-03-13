{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Omnidict.Language where

import           Relude

import           Control.Lens                   ( (.~)
                                                , (^?)
                                                )
import           Data.Aeson                     ( Value )
import           Data.Aeson.Lens
import           Data.Aeson.QQ
import qualified Network.Wreq                  as HTTP

import qualified Data.ByteString.Char8         as BS
import           Data.FileEmbed
import           Data.String.Interpolate        ( i )
import qualified Data.Text                     as T


(.!) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.!) = (.) . (.)


ai21Token :: ByteString
ai21Token = "Bearer " <> BS.strip $(embedFile "sensitive/ai21.txt")


fillPrompt :: Text -> [(Text, Text)] -> Text
fillPrompt = foldr (\(k, v) -> T.replace [i|[[#{k}]]|] v)

completePrompt :: Text -> [(Text, Text)] -> IO Text
completePrompt = flip getJ1 "\n" .! fillPrompt


getJ1 :: Text -> Text -> IO Text
getJ1 prompt stopAt = do
  res :: HTTP.Response Value <- HTTP.asJSON =<< HTTP.postWith
    (HTTP.defaults & HTTP.header "Authorization" .~ [ai21Token])
    "https://api.ai21.com/studio/v1/j1-jumbo/complete"
    [aesonQQ|
      {
        "prompt": #{prompt},
        "maxTokens": 128,
	"stopSequences": [#{stopAt}],
        "temperature": 1,
        "topP": 0.9,
        "presencePenalty": {"scale": 0.3}
      }
    |]

  let outputMay =
        res
          ^? ( HTTP.responseBody
             . key "completions"
             . nth 0
             . key "data"
             . key "text"
             . _String
             )
  case outputMay of
    Just output -> pure output
    Nothing     -> fail "Could not get output"
