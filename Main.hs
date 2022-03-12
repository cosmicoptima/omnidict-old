{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import           Relude
import qualified Relude.Unsafe                 as Unsafe

import           Omnidict.Language

import           Discord
import           Discord.Internal.Rest.Prelude  ( Request )
import           Discord.Requests
import           Discord.Types

import           Data.FileEmbed
import qualified Data.Text                     as T


-- files

qaPrompt :: Text
qaPrompt = (T.strip . decodeUtf8) $(embedFile "assets/qa.prompt")

discordToken_ :: Text
discordToken_ = decodeUtf8 $(embedFile "sensitive/discord.txt")


-- misc utilities

requireRight :: (Show a, MonadIO m) => Either a b -> m b
requireRight = either (die . show) pure

voiceFilter :: Text -> Text
voiceFilter = (<> "__**") . ("**__" <>) . T.toUpper . T.strip


-- discord utilities

type DH = DiscordHandler

type ChannelID = ChannelId -- evil

call :: (FromJSON a, Request (r a)) => r a -> DH a
call = restCall >=> requireRight

generalChannelID :: ChannelID
generalChannelID = 878376227428245558

post :: ChannelID -> Text -> DH Message
post channel = call . CreateMessage channel . voiceFilter


-- main

onStart :: DH ()
onStart = void (post generalChannelID "Rise and shine bitches")

onEvent :: Event -> DH ()
onEvent = \case
  MessageCreate m@(messageContent -> "gm") ->
    void (post (messageChannelId m) "Gaming mode activated")

  MessageCreate m@(T.stripPrefix "!qa " . messageContent -> Just q) ->
    liftIO (completePrompt qaPrompt [("question", q)])
      >>= void
      .   post (messageChannelId m)
      .   Unsafe.head
      .   T.splitOn "------"

  _ -> pure ()

main :: IO ()
main = void $ runDiscord
  (def { discordToken   = discordToken_
       , discordOnStart = onStart
       , discordOnEvent = onEvent
       , discordOnLog   = putTextLn
       }
  )
