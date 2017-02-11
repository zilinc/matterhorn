{-# LANGUAGE TypeFamilies #-}

module Strings.Helpers where

import           Brick (AttrName)
import           Data.Monoid ((<>))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import           GHC.Exts (IsString(..), IsList(..))

data UIString = UIString (Seq UIStringFragment)
  deriving (Eq, Show)

instance IsString UIString where
  fromString s = UIString (Seq.singleton (fromString s))

instance IsList UIString where
  type Item UIString = UIStringFragment
  fromList = UIString . fromList
  toList (UIString l) = toList l

data UIStringFragment = UISFrag (Maybe AttrName) Text
  deriving (Eq, Show)

instance IsString UIStringFragment where
  fromString s = UISFrag Nothing (fromString s)

(%>) :: AttrName -> Text -> UIStringFragment
attr %> t = UISFrag (Just attr) t

-- | Currently a hack; make this better!
engPlur :: Int -> Text -> Text
engPlur 1 t = t
engPlur _ t = t <> "s"

-- | Esperanto plural
epoPlur :: Int -> Text -> Text
epoPlur 1 t = t
epoPlur _ t = t <> "j"
