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
        stateDyn :: Dynamic t GameState <- foldDyn (\m (b,p) -> (\bo -> (nim (bestMove bo) bo, not p)) $ nim m b) (nimState,True) evt
        let wha = buttons <$> stateDyn
        huh :: Event t (Event t Move) <- networkView wha
        evt :: Event t Move <- switchHold never huh
        bom <- holdDyn (0,0) evt
        el "h1" $ dynText $ fmap tshow $ bom
        el "p" $ dynText $ fmap (tshow . getAllMoves . fst) stateDyn
      return ()
  }

buttons ::
  ( DomBuilder t m
  , PostBuild t m) => GameState -> m (Event t Move)
buttons (nimState,player) = do
    events :: [Event t Move] <- forM (zip [0..] nimState)
      $ \(pile, pileSize) -> divClass "row" $ do
      events <- forM (reverse $ take (fromEnum pileSize) [1..]) $ \bead -> do
        let move = (pile, bead)
        (btn,_) <- elAttr' "button" ("class" =: "button") $ text "+"
        return $ (move <$ domEvent Click btn)
        
      return $ leftmost events
    return $ leftmost events

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
