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
import Control.Monad.Fix
import qualified Data.Text as T
import Data.Bits

-- | (_,True) if it's the players turn
type GameState = (Board,Bool)
type Board = [Integer]
type Move = (Integer,Integer)

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
      let nimState = reverse [1..6]
      rec
        -- understandable AI implementation:
        -- stateDyn :: Dynamic t GameState <-
        --   foldDyn (\m (b,p) -> (\bo -> (nim (bestMove bo) bo, not p)) $ nim m b) (nimState,True) clickEvent
        stateDyn :: Dynamic t GameState <-
          foldDyn (\m (b,p) -> (\bo -> (bo, not p)) $ nim m b) (nimState,True) clickEvent
        let wha = buttons <$> stateDyn
        huh :: Event t (Event t Move, Event t Move) <- networkView wha
        hoverEvent :: Event t Move <- switchHold never $ fst <$> huh
        clickEvent :: Event t Move <- switchHold never $ snd <$> huh
        bom <- holdDyn (0,0) $ clickEvent
        bim <- holdDyn (0,0) $ hoverEvent
        el "h1" $ dynText $ fmap tshow $ bom
        el "p" $ dynText $ fmap tshow $ bim
      return ()
  }

buttons ::
  ( DomBuilder t m
  , PostBuild t m
  , MonadHold t m
  , MonadFix m) => GameState -> m (Event t Move, Event t Move)
buttons (nimState,player) = do
    events <- forM (zip [0..] nimState) $ \(pile, pileSize) -> divClass "row" $ do
      let lamb = (
            \(accHover, accOut, accClick) bead -> do
              let move = (pile, bead)
              let buttonAttrs = ("class" =: "button")
              rec
                let hoverAttrs = (buttonAttrs <> ("style" =: "background-color:tomato")) <$ (leftmost [hoverEvent, () <$ accHover])
                let outAttrs = buttonAttrs <$ (leftmost [outEvent, () <$ accOut])
                dynAttrs <- holdDyn buttonAttrs $ hoverAttrs <> outAttrs
                let ( hoverEvent
                      , outEvent
                      , clickEvent
                      ) = ( domEvent Mouseover btn
                          , domEvent Mouseout btn
                          , domEvent Click btn
                          )
                (btn,_) <- elDynAttr' "button" dynAttrs $ text "+"
              let (finalHover,finalOut,finalClick) = bother (move <$) (hoverEvent, outEvent, clickEvent)
              return $ ( leftmost [accHover, finalHover]
                       , leftmost [accOut, finalOut]
                       , leftmost [accClick, finalClick]
                       )
                 )
      (hoverEvent,outEvent,clickEvent) <- foldM lamb (never,never,never) (reverse $ take (fromEnum pileSize) [1..])
      return (hoverEvent,clickEvent)
    return $ both leftmost $ unzip events

nim :: Move -> Board -> Board
nim _ [] = []
nim (pile, beads) (x:xs)
  | pile == 0 = (x-beads : xs)
  | otherwise = (x:nim (pile-1, beads) xs)

getAllMoves :: Board -> [Move]
getAllMoves board = [(pile,bead) | (pile,beads) <- zip [0..] board, bead <- [1..beads]]


bestMove :: Board -> Move
bestMove board = case goodMoves of
                      [] -> head moves
                      (x:_) -> x
  where moves = getAllMoves board
        goodMoves = filter (\x -> (foldr xor 0 (nim x board))==0) moves

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

both :: (a -> b) -> (a,a) -> (b,b)
both f (a,b) = (f a, f b)

bother :: (a -> b) -> (a,a,a) -> (b,b,b)
bother f (a,b,c) = (f a, f b, f c)
