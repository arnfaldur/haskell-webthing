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
import Control.Monad.Trans
import Control.Monad.Fix
import qualified Data.Text as T
import Data.Bits
import Data.Time

-- | (_,True) if it's the players turn
type GameState = (Board,Player)
type Board = [Integer]
type Move = (Integer,Integer)
data Player = User | DrNim deriving Eq

other :: Player -> Player
other User = DrNim
other DrNim = User

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

      el "h1"  $ text "Welcome to Nim"

      el "div" $ do
          el "p" $ text "Try to beat Dr. Nim, the master of all nimians."
          el "p" $ text "Win by taking the last bead."

      --eBtn <- button "Start"

      let buttonAttrs = ("class" =: "button")
      rec
        startButtonAttrs <- foldDyn ($) buttonAttrs . mergeWith (.) $
          [ (\old -> old <> ("style" =: "visibility:hidden;transition-duration:1ms")) <$ eBtn
          , (\_ -> buttonAttrs) <$ eGameOver
          ]
        (elBtn, _) <- do
          (innerElBtn,_) <- elDynAttr' "button" startButtonAttrs $ text "Start"
          return $ (innerElBtn,1)

        let eBtn = domEvent Click elBtn

        dBool <- toggle False (domEvent Click elBtn)

      -- el "h1" $ dynText $ fmap tshow $ dBool

        eTick <- fmap (switch . current) $ prerender (return never) $ do
          now <- liftIO getCurrentTime
          eTick <- tickLossy 1 now
          return eTick

      --if
        let debug = False

        -- understandable AI implementation:
        -- dState :: Dynamic t GameState <-
        -- foldDyn (\m (b,p) -> (\bo -> (nim (bestMove bo) bo, not p)) $ nim m b) (nimState,True) eBeadClick
        gameText <-
          foldDyn ($) "" . mergeWith (.) $ [
              (\_ -> "") <$ eBtn
            , (\(_, p) -> \_ -> "Game Over, You " ++ if p == User then "have been nimed" else "are the nimest!") <$> eGameOver
          ]

        el "h1" $ dynText $ fmap T.pack gameText

        let eStateUpdate = updated dState
        let eGameOver    = ffilter (emptyBoard . fst) eStateUpdate
        let ePlayerMoved = ffilter (\p -> DrNim == snd p) eStateUpdate
        let eDrNimMoved  = ffilter (\p -> User == snd p) eStateUpdate

        eDrNimMove <-
             switchHold never $ leftmost [ eTick <$ ePlayerMoved
                                         , never <$ eStateUpdate
                                         ]
        ePlayerMove <-
             switchHold never $ leftmost [ eBeadClick <$ eDrNimMoved
                                         , never <$ eStateUpdate
                                         ]

        dState :: Dynamic t GameState <-
          foldDyn ($) ([], User) . mergeWith (.) $ [ fmap (\m (b,p) -> (nim m b, other p)) ePlayerMove
                                                   , fmap (\_ (b,p) -> (nim (bestMove b) b, other p)) eDrNimMove
                                                   , (\_ -> (reverse [1..5], User)) <$ eBtn
                                                   ]

        huh :: Event t (Event t Move, Event t Move) <- networkView $ buttons <$> dState
        eBeadHover :: Event t Move <- switchHold never $ fst <$> huh
        eBeadClick :: Event t Move <- switchHold never $ snd <$> huh

        if debug then do
          bom <- holdDyn (0,0) $ eBeadClick
          bim <- holdDyn (0,0) $ eBeadHover
          el "h1" $ dynText $ fmap tshow $ bom
          el "p" $ dynText $ fmap tshow $ bim
        else do return ()
      return ()
  }

-- ticker :: () => NominalDiffTime -> Event t TickInfo
-- ticker time = do
--   now <- liftIO getCurrentTime
--   return (tickLossy time now)

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
                let hoverAttrs = (buttonAttrs <> ("style" =: "background-color:tomato")) <$ (leftmost [eBeadHover, () <$ accHover])
                let outAttrs = buttonAttrs <$ (leftmost [eBeadOut, () <$ accOut])
                dynAttrs <- holdDyn buttonAttrs $ hoverAttrs <> outAttrs
                let ( eBeadHover
                      , eBeadOut
                      , eBeadClick
                      ) = ( domEvent Mouseover btn
                          , domEvent Mouseout btn
                          , domEvent Click btn
                          )
                (btn,_) <- elDynAttr' "button" dynAttrs $ text "+"
              let [finalHover,finalOut,finalClick] = map (move <$) [eBeadHover, eBeadOut, eBeadClick]
              return $ ( leftmost [accHover, finalHover]
                       , leftmost [accOut, finalOut]
                       , leftmost [accClick, finalClick]
                       )
                 )
      (eBeadHover,_eBeadOut,eBeadClick) <- foldM lamb (never,never,never) (reverse $ take (fromEnum pileSize) [1..])
      return (eBeadHover,eBeadClick)
    return $ both leftmost $ unzip events

emptyBoard :: Board -> Bool
emptyBoard []   = True
emptyBoard (x:xs) = if x == 0 then emptyBoard xs else False

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
