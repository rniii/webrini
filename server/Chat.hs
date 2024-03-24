{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Chat (initChat) where

import Control.Concurrent.MVar
import Control.Exception (finally)
import Control.Lens
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IntMap
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Network.HTTP.Types (badRequest400)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (Connection, acceptRequest, defaultConnectionOptions, receiveData, sendTextData, withPingThread)
import System.Random.Stateful

data Client = Client
    { _clientConn :: Connection
    , _clientNick :: Text
    }

makeLenses ''Client

data State = State
    { _clients :: IntMap Client
    , _counter :: Int
    }

makeLenses ''State

data Msg source
    = Chat {src :: source, text :: Text}
    | Comm {name :: Text, args :: [Text]}
    deriving (Generic)

type ServerMsg = Msg Text
type ClientMsg = Msg ()

instance ToJSON ServerMsg
instance FromJSON ClientMsg

data Hello = Hello
    { nick :: Maybe Text
    }
    deriving (Generic)

instance FromJSON Hello

initChat :: IO Application
initChat =
    chat <$> newMVar (State IntMap.empty 0)

chat :: MVar State -> Application
chat state = websocketsOr defaultConnectionOptions server reject
  where
    server pending = do
        conn <- acceptRequest pending

        withPingThread conn 30 (return ()) $
            runConnection state conn

    reject _ respond =
        respond $ responseLBS badRequest400 [] "Expected websocket upgrade request"

runConnection :: MVar State -> Connection -> IO ()
runConnection state conn = do
    Hello{nick = chosenNick} <- recv conn
    client <- Client conn <$> maybe randomNick return chosenNick

    i <- newClient client & modifyMVar state

    flip finally (return . killClient i & modifyMVar_ state) $ do
        readMVar state >>= flip broadcast (Comm "join" [client ^. clientNick])

        forever $ do
            msg <- recv conn

            case msg of
                Chat () m -> do
                    state' <- readMVar state
                    broadcast state' $ Chat ((state' ^. clients) ! i ^. clientNick) m
                Comm "nick" (nick : _) ->
                    return . updateClient i (clientNick .~ nick) & modifyMVar_ state
                Comm cmd _ ->
                    send conn $ Chat "Server" ("/" <> cmd <> ": Unknown command")

randomNick :: IO Text
randomNick =
    ("anon" <>) . Text.pack . show <$> uniformRM (1000, 9999 :: Int) globalStdGen

newClient :: Client -> (State -> IO (State, Int))
newClient client state = do
    return (state', i)
  where
    i = state ^. counter
    state' =
        state
            & counter +~ 1
            & clients %~ IntMap.insert i client

killClient :: Int -> (State -> State)
killClient i =
    clients %~ IntMap.delete i

updateClient :: Int -> (Client -> Client) -> (State -> State)
updateClient i f =
    clients %~ IntMap.adjust f i

broadcast :: State -> ServerMsg -> IO ()
broadcast state msg =
    mapM_ (\c -> send (c ^. clientConn) msg) (state ^. clients)

send :: Connection -> ServerMsg -> IO ()
send c m =
    sendTextData c $ encode m

recv :: (FromJSON a) => Connection -> IO a
recv c =
    receiveData c >>= either fail return . eitherDecode
