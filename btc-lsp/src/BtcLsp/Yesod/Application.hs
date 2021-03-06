{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module BtcLsp.Yesod.Application
  ( appMain,
  )
where

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!

import qualified BtcLsp.Class.Env as Class
import BtcLsp.Yesod.Handler.About
import BtcLsp.Yesod.Handler.Common
import BtcLsp.Yesod.Handler.Home
import BtcLsp.Yesod.Handler.Language
import BtcLsp.Yesod.Handler.OpenChan
import BtcLsp.Yesod.Handler.SwapIntoLnCreate
import BtcLsp.Yesod.Handler.SwapIntoLnSelect
import BtcLsp.Yesod.Handler.SwapUpdates
import BtcLsp.Yesod.Import
import Control.Monad.Logger (liftLoc)
import Language.Haskell.TH.Syntax (qLocation)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.Wai (Middleware, pathInfo)
import Network.Wai.Handler.Warp
  ( Settings,
    defaultSettings,
    defaultShouldDisplayException,
    runSettings,
    setHost,
    setOnException,
    setPort,
  )
import Network.Wai.Middleware.RequestLogger
  ( Destination (Logger),
    DetailedSettings (..),
    OutputFormat (..),
    destination,
    mkRequestLogger,
    outputFormat,
  )
import System.Log.FastLogger
  ( defaultBufSize,
    newStdoutLoggerSet,
    toLogStr,
  )

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation ::
  ( Class.Env m
  ) =>
  Pool SqlBackend ->
  UnliftIO m ->
  AppSettings ->
  IO App
makeFoundation sqlPool appMRunner appSettings = do
  -- Some basic initializations: HTTP connection manager, logger, and static
  -- subsite.
  appHttpManager <- getGlobalManager
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  appStatic <-
    (if appMutableStatic appSettings then staticDevel else static)
      (appStaticDir appSettings)

  -- We need a log function to create a connection pool. We need a connection
  -- pool to create our foundation. And we need our foundation to get a
  -- logging function. To get out of this loop, we initially create a
  -- temporary foundation without a real connection pool, get a log function
  -- from there, and then create the real foundation.
  let mkFoundation appConnPool = App {..}
  -- The App {..} syntax is an example of record wild cards. For more
  -- information, see:
  -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html

  -- Return the foundation
  return $ mkFoundation sqlPool

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: YesodLog -> App -> IO Application
makeApplication yesodLog foundation = do
  logWare <- makeLogWare yesodLog foundation
  -- Create the WAI application and apply middlewares
  appPlain <- toWaiAppPlain foundation
  return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: YesodLog -> App -> IO Middleware
makeLogWare yesodLog foundation =
  mkRequestLogger
    def
      { outputFormat =
          DetailedWithSettings $
            def
              { useColors = True,
                mFilterRequests = Just $ reqFilter yesodLog
              },
        destination =
          Logger
            . loggerSet
            $ appLogger foundation
      }
  where
    reqFilter YesodLogAll _ =
      const True
    reqFilter YesodLogNothing _ =
      const False
    reqFilter YesodLogNoMain req =
      const $
        case pathInfo req of
          [] -> False
          x : _ ->
            x
              `notElem` [ "static",
                          "favicon.ico",
                          "robots.txt"
                        ]

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
  setPort (appPort $ appSettings foundation) $
    setHost (appHost $ appSettings foundation) $
      setOnException
        ( \_req e ->
            when (defaultShouldDisplayException e) $
              messageLoggerSource
                foundation
                (appLogger foundation)
                $(qLocation >>= liftLoc)
                "yesod"
                LevelError
                (toLogStr $ "Exception from Warp: " ++ show e)
        )
        defaultSettings

-- | The @main@ function for an executable running this site.
appMain ::
  ( Class.Env m
  ) =>
  YesodLog ->
  Pool SqlBackend ->
  UnliftIO m ->
  IO ()
appMain yesodLog sqlPool appMRunner = do
  -- Get the settings from all relevant sources
  settings <-
    loadYamlSettingsArgs
      -- fall back to compile-time values, set to [] to require values at runtime
      [configSettingsYmlValue]
      -- allow environment variables to override
      useEnv

  -- Generate the foundation from the settings
  foundation <- makeFoundation sqlPool appMRunner settings

  -- Generate a WAI Application from the foundation
  app <- makeApplication yesodLog foundation

  -- Run the application with Warp
  runSettings (warpSettings foundation) app
