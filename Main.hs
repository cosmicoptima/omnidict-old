{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import           Relude

import           Omnidict.Language

import           Discord
import           Discord.Internal.Rest.Prelude  ( Request )
import           Discord.Requests
import           Discord.Types

import           Data.FileEmbed
import qualified Data.Text                     as T
import           System.Random                  ( randomIO )


-- files

deathPrompt :: Text
deathPrompt = (T.strip . decodeUtf8) $(embedFile "assets/death.prompt")

qaPrompt :: Text
qaPrompt = (T.strip . decodeUtf8) $(embedFile "assets/qa.prompt")

discordToken_ :: Text
discordToken_ = decodeUtf8 $(embedFile "sensitive/discord.txt")


-- misc utilities

requireRight :: (Show a, MonadIO m) => Either a b -> m b
requireRight = either (die . show) pure

voiceFilter :: Text -> Text
voiceFilter = (<> "__**") . ("**__" <>) . T.toUpper . T.strip

odds :: MonadIO m => Double -> m () -> m ()
odds p action = liftIO randomIO >>= flip when action . (< p)


-- discord utilities

type DH = DiscordHandler

type ChannelID = ChannelId -- evil

call :: (FromJSON a, Request (r a)) => r a -> DH a
call = restCall >=> requireRight

generalChannelID :: ChannelID
generalChannelID = 878376227428245558

post :: ChannelID -> Text -> DH Message
post channel = call . CreateMessage channel . voiceFilter


-- dictposting

deathPost :: ChannelID -> DH ()
deathPost channel = do
  liftIO (completePrompt deathPrompt []) >>= void . post channel

qaPost :: ChannelID -> Text -> DH ()
qaPost channel question =
  liftIO (completePrompt qaPrompt [("question", question)])
    >>= void
    .   post channel


-- main

onStart :: DH ()
onStart = void (post generalChannelID "Rise and shine bitches")

onEvent :: Event -> DH ()
onEvent = \case
  MessageCreate m@(messageContent -> "gm") ->
    void (post (messageChannelId m) "Gaming mode activated")

  MessageCreate m@(messageContent -> "how will i die") ->
    deathPost (messageChannelId m)

  MessageCreate m@(T.stripPrefix "dict, " . messageContent -> Just q) ->
    qaPost (messageChannelId m) q

  MessageCreate m -> odds 0.02 $ qaPost (messageChannelId m) (messageContent m)

  _               -> pure ()

main :: IO ()
main = void $ runDiscord
  (def { discordToken   = discordToken_
       , discordOnStart = onStart
       , discordOnEvent = onEvent
       , discordOnLog   = putTextLn
       }
  )
