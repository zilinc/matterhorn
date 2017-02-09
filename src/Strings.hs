module Strings where

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Lens.Micro.Platform

import           Types

get :: StringMessage s => ChatState -> s -> Text
get st m = stringOf (st^.csResources.crConfiguration) m

class StringMessage s where
  stringOf :: Config -> s -> Text

data UIString
  = SChannels
  | SUsers
  | SReplyPrompt
  | SEditPrompt
  | SMultiLineMode
  | SMultiLinePrompt Int
  | SNotConnected
  | SLoadingChannelScrollback
  | SRefreshingScrollback
  | SLoadMoreMessages
  | SNewMessages
  | SBotUser Text
  | SAttachment Text
  | SReply
  | SEdit
  | SDelete
  | SOpenUrls Int
  | SYank
  | SMessageSelect
  | SDeleteConfirm
  | SURLs
  | SNoUrls

-- | Currently a hack; make this better!
engPlur :: Int -> Text -> Text
engPlur 1 t = t
engPlur _ t = t <> "s"

-- | Esperanto plural
epoPlur :: Int -> Text -> Text
epoPlur 1 t = t
epoPlur _ t = t <> "j"

instance StringMessage UIString where
  stringOf cf msg = case configLanguage cf of
    "epo" -> case msg of
      SChannels -> "Kanaloj"
      SUsers -> "Uzantoj"
      SReplyPrompt -> "respondi"
      SEditPrompt -> "redakti"
      SMultiLineMode -> "Je multliniamodo. Premu M-e per fini."
      SMultiLinePrompt n ->
        "[" <> T.pack (show n) <> " " <> epoPlur n "linio" <> "; Enigo: sendi, M-e: redakti, Retropaŝo: nuligi] "
      SNotConnected -> "[MALKONEKTATE]"
      SLoadingChannelScrollback -> "[Ŝargante kanalantaŭlogon...]"
      SRefreshingScrollback -> "[Freŝigante kanalantaŭlogon...]"
      SLoadMoreMessages -> "<< Premu C-b per ŝargi plijn mesaĝojn >>"
      SNewMessages -> "Novaj Mesaĝoj"
      SBotUser u -> u <> "[ROBOTO]"
      SAttachment a -> "  [afiksata: `" <> a <> "`]"
      SReply -> "respondi"
      SEdit  -> "redakti"
      SDelete -> "viŝi"
      SOpenUrls n ->
        "apertu " <> T.pack (show n) <> " " <> engPlur n "URL"
      SYank -> "kopii"
      SMessageSelect -> "Mesaĝelektaĵo: "
      SDeleteConfirm -> "Ĉu vi estas certa, ke vi volas viŝi la elektitajn mesaĝojn? (y/n)"
      SURLs -> "URLs: "
      SNoUrls -> "Neniu adresoj trovitaj je tiu ĉi kanelo."
    _ -> case msg of
      SChannels -> "Channels"
      SUsers -> "Users"
      SReplyPrompt -> "reply"
      SEditPrompt -> "edit"
      SMultiLineMode -> "In multi-line mode. Press M-e to finish."
      SMultiLinePrompt n ->
        "[" <> T.pack (show n) <> " " <> engPlur n "line" <> "; Enter: send, M-e: edit, Backspace: cancel] "
      SNotConnected -> "[NOT CONNECTED]"
      SLoadingChannelScrollback -> "[Loading channel scrollback...]"
      SRefreshingScrollback -> "[Refreshing scrollback...]"
      SLoadMoreMessages -> "<< Press C-b to load more messages >>"
      SNewMessages -> "New Messages"
      SBotUser u -> u <> "[BOT]"
      SAttachment a -> "  [attached `" <> a <> "`]"
      SReply -> "reply"
      SEdit  -> "edit"
      SDelete -> "delete"
      SOpenUrls n ->
        "open " <> T.pack (show n) <> " " <> engPlur n "URL"
      SYank -> "yank"
      SMessageSelect -> "Message select: "
      SDeleteConfirm -> "Are you sure you want to delete the selected message? (y/n)"
      SURLs -> "URLs: "
      SNoUrls -> "No URLs found in this channel."

data ErrorMessage
  = SErrLeaveDirectMessage
    -- ^ ""
  | SErrNoTarget Text
    -- ^ "No channel or user named []"
  | SErrUnknownCommand Text
    -- ^ "Unknown command: []"
  | SErrInvalidCommand Text
    -- ^ "Invalid command: []"
  | SErrRunningCommand Text
    -- ^ "Error running command: []"
  | SErrMissingURLOpen
    -- ^ "Config option 'urlOpenCommand' missing; cannot open URL."

data MessageString
  = SAvailableThemes [Text]
    -- ^ "Available built-in themes:"
  | SChannelJoin Text
  | SChannelLeave Text

instance StringMessage MessageString where
  stringOf cf msg = case configLanguage cf of
    "epo" -> case msg of
      _ -> "malimplemata"
    _ -> case msg of
      SAvailableThemes as -> T.intercalate "\n\n" ("Available themes:" : (("  " <>) <$> as))
      SChannelJoin u -> undefined
      SChannelLeave u -> undefined
