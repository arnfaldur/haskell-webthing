{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend where

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom
import Reflex.Network

import Common.Route

import Control.Monad
import qualified Data.Text as T

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Nim Game"
      elAttr "link" ("href" =: static @"main.css"
                     <> "type" =: "text/css"
                     <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      let nimState = reverse [1..7]
      rec
        stateDyn :: Dynamic t [Integer] <- foldDyn (flip nim) nimState evt
        let wha = buttons <$> stateDyn
        huh :: Event t (Event t (Integer,Integer)) <- networkView $ wha
        evt :: Event t (Integer,Integer) <- switchHoldPromptly never $ huh
      return ()
  }

buttons ::
  ( DomBuilder t m
  , PostBuild t m) => [Integer] -> m (Event t (Integer,Integer))
buttons nimState = do
    events :: [Event t (Integer,Integer)] <- forM (zip [0..] nimState)
      $ \(pile, pileSize) -> divClass "row" $ do
      events <- forM (reverse $ take (fromEnum pileSize) [1..]) $ \bead -> do
        let move = (pile, bead)
        (btn,_) <- elAttr' "button" ("class" =: "button") $ text "#"
        return $ (move <$ domEvent Click btn)
      return $ leftmost events
    return $ leftmost events

nim :: [Integer] -> (Integer, Integer) -> [Integer]
nim [] _ = []
nim (x:xs) (pile, beads)
  | pile == 0 = (x-beads : xs)
  | otherwise = (x:nim xs (pile-1, beads))
