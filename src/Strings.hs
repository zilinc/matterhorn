{-# LANGUAGE TemplateHaskell #-}

module Strings where

import           Data.Monoid ((<>))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import           Lens.Micro.Platform

data Translation = Translation
  { _trMsgs :: UIMessages
  , _trErrs :: UIErrors
  }

data UIMessages = UIMessages
  { _sChannels                 :: Text
  , _sUsers                    :: Text
  , _sReplyPrompt              :: Text
  , _sEditPrompt               :: Text
  , _sMultiLineMode            :: Text
  , _sMultiLinePrompt          :: Int -> Text
  , _sNotConnected             :: Text
  , _sLoadingChannelScrollback :: Text
  , _sRefreshingScrollback     :: Text
  , _sLoadMoreMessages         :: Text
  , _sNewMessages              :: Text
  , _sBotUser                  :: Text -> Text
  , _sAttachment               :: Text -> Text
  , _sReply                    :: Text
  , _sEdit                     :: Text
  , _sDelete                   :: Text
  , _sOpenUrls                 :: Int -> Text
  , _sYank                     :: Text
  , _sMessageSelect            :: Text
  , _sDeleteConfirm            :: Text
  , _sURLs                     :: Text
  , _sNoUrls                   :: Text
  }

data UIErrors = UIErrors
  { _sErrLeaveDirectMessage :: Text
    -- ^ ""
  , _sErrNoTarget :: Text -> Text
    -- ^ "No channel or user named []"
  , _sErrUnknownCommand :: Text -> Text
    -- ^ "Unknown command: []"
  , _sErrInvalidCommand :: Text -> Text
    -- ^ "Invalid command: []"
  , _sErrRunningCommand :: Text -> Text
    -- ^ "Error running command: []"
  , _sErrMissingURLOpen :: Text
    -- ^ "Config option 'urlOpenCommand' missing; cannot open URL."
  }

makeLenses ''Translation
makeLenses ''UIMessages
makeLenses ''UIErrors

engMessages :: Translation
engMessages = Translation
  { _trMsgs = UIMessages
      { _sChannels = "Channels"
      , _sUsers = "Users"
      , _sReplyPrompt = "reply"
      , _sEditPrompt = "edit"
      , _sMultiLineMode = "In multi-line mode. Press M-e to finish."
      , _sMultiLinePrompt = \ n ->
          ("[" <> T.pack (show n) <>
           " " <> engPlur n "line" <>
           "; Enter: send, M-e: edit, Backspace: cancel] ")
      , _sNotConnected = "[NOT CONNECTED]"
      , _sLoadingChannelScrollback = "[Loading channel scrollback...]"
      , _sRefreshingScrollback = "[Refreshing scrollback...]"
      , _sLoadMoreMessages = "<< Press C-b to load more messages >>"
      , _sNewMessages = "New Messages"
      , _sBotUser = \ u -> u <> "[BOT]"
      , _sAttachment = \ a -> "  [attached `" <> a <> "`]"
      , _sReply = "reply"
      , _sEdit  = "edit"
      , _sDelete = "delete"
      , _sOpenUrls = \ n ->
          "open " <> T.pack (show n) <> " " <> engPlur n "URL"
      , _sYank = "yank"
      , _sMessageSelect = "Message select: "
      , _sDeleteConfirm = "Are you sure you want to delete the selected message? (y/n)"
      , _sURLs = "URLs: "
      , _sNoUrls = "No URLs found in this channel."
      }
  , _trErrs = UIErrors
      { _sErrLeaveDirectMessage = ""
      , _sErrNoTarget = \ t -> "No channel or user named " <> t
      , _sErrUnknownCommand = \ t -> "Unknown command: " <> t
      , _sErrInvalidCommand = \ t -> "Invalid command: " <> t
      , _sErrRunningCommand = \ t -> "Error running command: " <> t
      , _sErrMissingURLOpen = "Config option 'urlOpenCommand' missing: cannot open URL."
      }
  }

epoMessages :: Translation
epoMessages = Translation
  { _trMsgs = UIMessages
      { _sChannels = "Kanaloj"
      , _sUsers = "Uzantoj"
      , _sReplyPrompt = "respondi"
      , _sEditPrompt = "redakti"
      , _sMultiLineMode = "Je multliniamodo. Premu M-e per fini."
      , _sMultiLinePrompt = \ n ->
    "[" <> T.pack (show n) <> " " <> epoPlur n "linio" <> "; Enigo: sendi, M-e: redakti, Retropaŝo: nuligi] "
      , _sNotConnected = "[MALKONEKTATE]"
      , _sLoadingChannelScrollback = "[Ŝargante kanalantaŭlogon...]"
      , _sRefreshingScrollback = "[Freŝigante kanalantaŭlogon...]"
      , _sLoadMoreMessages = "<< Premu C-b per ŝargi plijn mesaĝojn >>"
      , _sNewMessages = "Novaj Mesaĝoj"
      , _sBotUser = \ u -> u <> "[ROBOTO]"
      , _sAttachment = \ a -> "  [afiksata: `" <> a <> "`]"
      , _sReply = "respondi"
      , _sEdit  = "redakti"
      , _sDelete = "viŝi"
      , _sOpenUrls = \ n ->
    "apertu " <> T.pack (show n) <> " " <> engPlur n "URL"
      , _sYank = "kopii"
      , _sMessageSelect = "Mesaĝelektaĵo: "
      , _sDeleteConfirm = "Ĉu vi estas certa, ke vi volas viŝi la elektitajn mesaĝojn? (y/n)"
      , _sURLs = "URLs: "
      , _sNoUrls = "Neniu adresoj trovitaj je tiu ĉi kanelo."
      }
  }

getTranslation :: Text -> Translation
getTranslation "epo" = epoMessages
getTranslation _     = engMessages

-- | Currently a hack; make this better!
engPlur :: Int -> Text -> Text
engPlur 1 t = t
engPlur _ t = t <> "s"

-- | Esperanto plural
epoPlur :: Int -> Text -> Text
epoPlur 1 t = t
epoPlur _ t = t <> "j"


data MessageString
  = SAvailableThemes [Text]
    -- ^ "Available built-in themes:"
  | SChannelJoin Text
  | SChannelLeave Text
