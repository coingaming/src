{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module BtcLsp.Yesod.Handler.OpenChan where

import BtcLsp.Class.Env
import BtcLsp.Yesod.Import

getOpenChanR :: Handler Html
getOpenChanR = do
  App {appMRunner = UnliftIO run} <- getYesod
  nodeUri <- liftIO $ run getLndNodeUri
  nodeUriHex <-
    eitherM
      (const badMethod)
      (pure . unNodeUriHex)
      . pure
      $ tryFrom nodeUri
  qrCodeSrc <-
    maybeM badMethod pure
      . pure
      $ toQr nodeUriHex
  panelLayout Info MsgOpenChanRLinkLong MsgOpenChanInstruction $ do
    setTitleI MsgOpenChanRTitle
    $(widgetFile "open_chan")
  where
    htmlUuid = $(mkHtmlUuid)
