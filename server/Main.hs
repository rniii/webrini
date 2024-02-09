{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
-- theres none already u silly
{-# OPTIONS_GHC -Wno-partial-fields #-}

{- |
Copyright : (c) 2024 rini
License   : Apache-2.0

webrini <https://rinici.de>
-}
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
import Network.Wai.Middleware.Gzip (GzipFiles (GzipIgnore, GzipPreCompressed), defaultGzipSettings, gzip, gzipFiles)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Network.Wai.Middleware.Static as Static
import Network.WebSockets (Connection, acceptRequest, defaultConnectionOptions, receiveData, sendTextData, withPingThread)
import System.Environment.Blank (getEnvDefault)
import System.Random.Stateful (globalStdGen, uniformRM)
import Text.Read (readMaybe)
import Web.Scotty (file, get, middleware, nested, notFound, scottyOpts, setHeader)
import qualified Web.Scotty as Scotty

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
    | Command !Text ![Text]
    deriving (Generic)

instance ToJSON ServerMsg
instance FromJSON ClientMsg

main :: IO ()
main = do
    port <- fromMaybe (error "Invalid $PORT") . readMaybe <$> getEnvDefault "PORT" "8080"
    chat' <- chat <$> newMVar (State IntMap.empty 0)
    staticCache <- Static.initCaching Static.PublicStaticCaching

    putStrLn "wheb life gives u mons .. u make monads...\n"

    scottyOpts (opts port) $ do
        middleware logStdoutDev
        middleware autohead
        middleware $ gzip gzipOpts
        middleware $ static staticCache

        get "/" $
            page "pages/index.html"
        get "/chat" $
            nested chat'
        notFound $
            page "pages/404.html"
  where
    opts port =
        Scotty.defaultOptions
            { Scotty.verbose = 0
            , Scotty.settings = setPort port defaultSettings
            }
    static cache =
        Static.staticPolicyWithOptions
            Static.defaultOptions
                { Static.cacheContainer = cache
                }
            $ Static.addBase "static"
    gzipOpts =
        defaultGzipSettings
            { gzipFiles = GzipPreCompressed GzipIgnore
            }

chat :: MVar State -> Application
chat state = websocketsOr defaultConnectionOptions server reject
  where
    server pending = do
        conn <- acceptRequest pending

        num <- uniformRM (1000, 9999 :: Int) globalStdGen
        let nick = "anon" <> Text.pack (show num)

        withPingThread conn 30 (return ()) $ do
            i <- modifyMVar state $ \state' -> do
                send conn $ motd state'
                broadcast state' $ Join nick

                let i = state' ^. counter
                let client = Client conn nick
                return (counter +~ 1 $ addClient i client state', i)

            let cleanup = modifyMVar_ state (return . killUser i)

            flip finally cleanup $ forever $ do
                msg <- recv conn >>= either fail return
                state' <- readMVar state

                case msg of
                    Send t ->
                        broadcast state' $ Chat nick t
                    Command "nick" (n : _) ->
                        modifyMVar_ state (return . updateClient i (username .~ n))
                    Command cmd _ ->
                        send conn $ Info $ "/" <> cmd <> ": Unknown command"

    reject _ respond =
        respond $ responseLBS badRequest400 [] "Expected websocket upgrade request"

-- | Serve a page
page :: FilePath -> Scotty.ActionM ()
page path = do
    setHeader "Content-Type" "text/html; charset=utf-8"
    file path

motd :: State -> ServerMsg
motd state =
    Info $ "hai ^^  ppl here:" <> names
  where
    names = Text.intercalate ", " $ fmap (^. username) (IntMap.elems (state ^. clients))

addClient :: Int -> Client -> (State -> State)
addClient i c =
    clients %~ IntMap.insert i c

killUser :: Int -> (State -> State)
killUser i =
    clients %~ IntMap.delete i

updateClient :: Int -> (Client -> Client) -> State -> State
updateClient i f =
    clients %~ IntMap.adjust f i

broadcast :: State -> ServerMsg -> IO ()
broadcast state msg =
    mapM_ (flip send msg . (^. connection)) (state ^. clients)

send :: Connection -> ServerMsg -> IO ()
send c m =
    sendTextData c $ encode m

recv :: Connection -> IO (Either String ClientMsg)
recv c =
    eitherDecode <$> receiveData c
