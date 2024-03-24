{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright : (c) 2024 rini
License   : Apache-2.0

webrini <https://rinici.de>
-}
module Main (main) where

import Chat (initChat)
import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Gzip (GzipFiles (..), defaultGzipSettings, gzip, gzipFiles)
import Network.Wai.Middleware.RequestLogger (logStdout)
import qualified Network.Wai.Middleware.Static as Static
import System.Environment.Blank (getEnvDefault)
import Text.Read (readMaybe)
import Web.Scotty (file, get, middleware, nested, notFound, scottyOpts, setHeader)
import qualified Web.Scotty as Scotty

main :: IO ()
main = do
    port <- fromMaybe (error "Invalid $PORT") . readMaybe <$> getEnvDefault "PORT" "8080"
    chat <- initChat
    staticCache <- Static.initCaching Static.PublicStaticCaching

    putStrLn "wheb life gives u mons .. u make monads...\n"

    scottyOpts (opts port) $ do
        middleware logStdout
        middleware autohead
        middleware $ gzip gzipOpts
        middleware $ static staticCache

        get "/" $
            page "pages/index.html"
        get "/chat" $
            nested chat
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

page :: FilePath -> Scotty.ActionM ()
page path = do
    setHeader "Content-Type" "text/html; charset=utf-8"
    file path
