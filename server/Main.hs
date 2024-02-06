{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
-- theres none already u silly
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Main (main) where

import Control.Concurrent.MVar
import Control.Exception (finally)
import Control.Lens
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Network.HTTP.Types (badRequest400)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Gzip (defaultGzipSettings, gzip)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Network.WebSockets (Connection, acceptRequest, defaultConnectionOptions, receiveData, sendTextData, withPingThread)
import System.Environment.Blank (getEnvDefault)
import System.Random (getStdGen, uniformR)
import Text.Read (readMaybe)
import Web.Scotty

data Client = Client
    { _connection :: Connection
    , _username :: Text
    }

makeLenses ''Client

data State = State
    { _clients :: IntMap Client
    , _counter :: Int
    }

makeLenses ''State

data ServerMsg
    = Chat {author :: !Text, text :: !Text}
    | Info {text :: !Text}
    | Join {user :: !Text}
    deriving (Generic)

data ClientMsg
    = Send {text :: !Text}
    | Command !Text
    deriving (Generic)

instance ToJSON ServerMsg
instance FromJSON ClientMsg

main :: IO ()
main = do
    port <- fromMaybe (error "Invalid $PORT") . readMaybe <$> getEnvDefault "PORT" "8080"
    let opts = Options{verbose = 0, settings = setPort port defaultSettings}

    chat' <- chat <$> newMVar (State IntMap.empty 0)

    scottyOpts opts $ do
        middleware logStdoutDev
        middleware autohead
        middleware $ gzip defaultGzipSettings
        middleware $ staticPolicy $ addBase "static"

        get "/" $ page "pages/index.html"
        get "/chat" $ nested chat'

chat :: MVar State -> Application
chat state = websocketsOr defaultConnectionOptions server reject
  where
    server pending = do
        conn <- acceptRequest pending

        gen <- getStdGen
        let (num, _) = uniformR (1000, 9999 :: Int) gen -- TODO: not
        let nick = "anon" `mappend` Text.pack (show num)

        withPingThread conn 30 (return ()) $ do
            i <- modifyMVar state $ \state' -> do
                let names = Text.intercalate ", " $ fmap (^. username) (IntMap.elems (state' ^. clients))
                send conn $ Info $ "[i] hai ^^  ppl here: " `mappend` names
                broadcast state' $ Join nick

                let i = state' ^. counter
                return (counter +~ 1 $ addClient i (Client conn nick) state', i)

            let cleanup = modifyMVar_ state $ \state' -> return $ killUser i state'

            flip finally cleanup $ forever $ do
                msg <- recv conn >>= either fail return
                state' <- readMVar state

                case msg of
                    Send t -> broadcast state' $ Chat nick t
                    Command _ -> fail "todo"

    reject _ respond =
        respond $ responseLBS badRequest400 [] "Expected websocket upgrade request"

page :: FilePath -> ActionM ()
page path = do
    setHeader "Content-Type" "text/html; charset=utf-8"
    file path

addClient :: Int -> Client -> (State -> State)
addClient i c =
    clients %~ IntMap.insert i c

killUser :: Int -> (State -> State)
killUser i =
    clients %~ IntMap.delete i

broadcast :: State -> ServerMsg -> IO ()
broadcast state msg =
    mapM_ (flip send msg . (^. connection)) (state ^. clients)

send :: Connection -> ServerMsg -> IO ()
send c m = sendTextData c $ encode m

recv :: Connection -> IO (Either String ClientMsg)
recv c = eitherDecode <$> receiveData c
