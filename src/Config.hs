{-# LANGUAGE RecordWildCards #-}

module Config (Config(..), getConfig) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Foreign.C.Types (CInt)
import           Scripting.Lua (LuaState)
import qualified Scripting.Lua as Lua
import           System.Exit (exitFailure)
import           System.Process (readProcess)

data Config = Config
  { configUser     :: Text
  , configHost     :: Text
  , configTeam     :: Text
  , configPort     :: Int
  , configPass     :: Text
  } deriving (Eq, Show)

getTableVal :: Lua.StackValue a => LuaState -> ByteString -> IO (Maybe a)
getTableVal l key = do
  Lua.pushstring l key
  Lua.gettable l 1
  res <- Lua.peek l 2
  Lua.pop l 1
  return res

connConf :: IORef Config -> LuaState -> IO CInt
connConf confRef l = do
  isTable <- Lua.istable l 1
  if not isTable
    then do
      putStrLn "Argument not a table"
      return 0
    else do
      Just configUser <- fmap decodeUtf8 `fmap` getTableVal l "user"
      Just configHost <- fmap decodeUtf8 `fmap` getTableVal l "host"
      Just configTeam <- fmap decodeUtf8 `fmap` getTableVal l "team"
      Just configPort <- getTableVal l "port"
      Just configPass <- fmap decodeUtf8 `fmap` getTableVal l "pass"
      writeIORef confRef Config { .. }
      return 0

getConfig :: IO Config
getConfig = do
  l <- Lua.newstate
  Lua.openlibs l
  Lua.newtable l
  conf <- newIORef Config
            { configUser = ""
            , configHost = ""
            , configTeam = ""
            , configPort = 443
            , configPass = ""
            }
  Lua.registerrawhsfunction l "connection" (connConf conf)
  Lua.loadfile l "config.lua"
  err <- Lua.pcall l 0 0 0
  if err > 0 then do
      rs <- Lua.tostring l (-1)
      BS.putStrLn rs
      exitFailure
    else do
      readIORef conf
