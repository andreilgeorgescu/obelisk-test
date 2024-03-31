{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Frontend where

import Control.Lens ((^.))
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.String
import Data.FileEmbed
import Language.Javascript.JSaddle (MonadJSM, eval, liftJSM, runJSM, JSVal, obj, jss, ToJSVal, JSM, fun, js, js1, jsg, js2, ghcjsPure, val)
import JSDOM (currentWindowUnchecked, currentDocumentUnchecked)
-- import Control.Monad.Trans.State (liftIO)

import Language.Javascript.JSaddle

import Control.Monad.IO.Class

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: "https://cdnjs.cloudflare.com/ajax/libs/tailwindcss/2.0.2/tailwind.min.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "script" ("type" =: "application/javascript" <> "src" =: "https://cdn.tailwindcss.com") blank
      elAttr "script" ("type" =: "application/javascript" <> "src" =: $(static "lib.js")) blank
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      -- el "h1" $ text "Welcome to Obelisk!"
      -- el "p" $ text $ T.pack commonStuff

      -- `prerender` and `prerender_` let you choose a widget to run on the server
      -- during prerendering and a different widget to run on the client with
      -- JavaScript. The following will generate a `blank` widget on the server and
      -- print "Hello, World!" on the client.
      prerender_ blank $ liftJSM $ void
        $ jsg ("window" :: T.Text)
        ^. js ("skeleton_lib" :: T.Text)
        ^. js1 ("log" :: T.Text) ("Hello, World!" :: T.Text)

      prerender_ blank $ void $ elDynHtml' "div" $ constDyn $(embedStringFile "frontend/src/test.html")

      -- evIncr <- loginClick
      el "h1" $ display =<< count =<< loginClick

      -- do
      --   getElementByIdUnchecked "login"
      --   el "h1" $ text "test"

      return ()
  }


-- showAlert :: (() -> JSM ()) -> JSM ()
-- showAlert onChange = do
--   return js1 ("alert" :: String) ("login clicked" :: String)

-- helper :: (a -> IO()) -> JSM ()
-- helper onChangeCallback = do
--   onChangeCallback ()
--   return js1 "void" 0

-- handleClick :: MonadJSM m => String -> m () -- (Event t0 ())
-- handleClick id = liftJSM $ do
--   doc <- currentDocumentUnchecked
--   loginButton <- doc ^. js1 ("getElementById" :: String)  (id :: String)
--   (onChangeEvent, onChangeCallback) <- newTriggerEvent
--   loginButton ^. js2 ("addEventListener" :: String) ("click" :: String) (fun $ \_ _ _ -> do
--     window <- currentWindowUnchecked
--     doc ^. js1 ("alert" :: String) ("login clicked" :: String)
--     liftIO $ onChangeCallback () -- pure liftIO onChangeCallback -- pure $ liftIO . onChangeCallback
--     return ())
--   return () -- onChangeEvent

-- loginClick :: MonadJSM m => m ()-- (Event t0 ())
-- loginClick = handleClick ("login" :: String)


handleClick :: String -> Event t0 ()
handleClick id = do
  (onChangeEvent, onChangeCallback) <- newTriggerEvent
  liftJSM $ do
    doc <- currentDocumentUnchecked
    loginButton <- doc ^. js1 ("getElementById" :: String)  (id :: String)
    loginButton ^. js2 ("addEventListener" :: String) ("click" :: String) (fun $ \_ _ _ -> do
      window <- currentWindowUnchecked
      doc ^. js1 ("alert" :: String) ("login clicked" :: String)
      liftIO $ onChangeCallback () -- pure liftIO onChangeCallback -- pure $ liftIO . onChangeCallback
      return ())
    return ()
  return onChangeEvent

-- loginClick :: MonadJSM m => m ()-- (Event t0 ())
loginClick :: Event t0 ()
loginClick = handleClick ("login" :: String)

-- Tailwind ui component found here: https://tailwindui.com/components/marketing/sections/heroes
-- HTML rendering example: https://srid.ca/obelisk-tutorial
