{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route


import Data.Text (Text)
import Control.Monad (forM)
import Control.Monad.Fix (MonadFix)
import Data.List (elem)
import Reflex.Dom
import Data.Array (Array, Ix)
import qualified Data.Array as A

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Typpa Example"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" $ text "Welcome to Nim!"
      el "p" $ text $ T.pack commonStuff
      elAttr "a" ("href" =: "https://wikipedia.org") $ text $ "This is some text"
      el "p" $ text $ T.pack commonStuff
      
      -- `prerender` and `prerender_` let you choose a widget to run on the server
      -- during prerendering and a different widget to run on the client with
      -- JavaScript. The following will generate a `blank` widget on the server and
      -- print "Hello, World!" on the client.
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)

      elAttr "img" ("src" =: static @"obelisk.jpg") blank
      el "div" $ do
        exampleConfig <- getConfig "common/example"
        case exampleConfig of
          Nothing -> text "No config file found in config/common/example"
          Just s -> text $ T.decodeUtf8 s
      app
      return ()
  }


data Row =
  Row_1
  | Row_2
  | Row_3
  deriving (Eq, Show, Ord, Enum, Bounded, Ix)

data Col =
  Col_1
  | Col_2
  | Col_3
  deriving (Eq, Show, Ord, Enum, Bounded, Ix)

data Player =
  Player_X
  | Player_O
  deriving (Eq, Show, Ord, Enum, Bounded)

winningPos :: [[(Row, Col)]]
winningPos =
  [[(a, b) | a <- [Row_1 .. ]] | b <- [Col_1 .. ]]
  <> [[(a, b) | b <- [Col_1 .. ]] | a <- [Row_1 .. ]]
  <> [ [(Row_1, Col_1), (Row_2, Col_2), (Row_3, Col_3)]
     , [(Row_1, Col_3), (Row_2, Col_2), (Row_3, Col_1)]
     ]

-- This is not really required, as the Moves is sufficient
type Squares = Array (Row, Col) (Maybe Player)
type MovePos = (Row, Col)
type Moves = [MovePos]

data State = State
  { state_currentMove :: Int
  , state_moves :: Moves
  }

data Action =
  Action_GoToMove Int
  | Action_DoMove MovePos

app
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => m ()
app = divClass "game" $ do
  let initState = State 0 []

  rec
    let
      squares = ffor stateDyn $ \(State p mvs) -> makeSquares $ take p mvs
    stateDyn <- foldDynMaybe changeGameState initState $
      leftmost [goToMove, newMove]
    newMove <- gameBoard squares
    goToMove <- gameInfo stateDyn
  return ()

makeSquares :: Moves -> Squares
makeSquares mvs = A.accumArray (flip const) Nothing ((Row_1, Col_1), (Row_3, Col_3)) ls
  where
    ls = zip mvs $ concat $ repeat [Just Player_X, Just Player_O]

changeGameState :: Action -> State -> Maybe State
changeGameState a (State p moves) = case a of
  (Action_GoToMove i) -> Just $ State i moves
  (Action_DoMove m) -> case (checkWon curMoves, elem m curMoves) of
    (Nothing, False) -> Just $ State (p + 1) newMoves
    _ -> Nothing
    where
      newMoves = curMoves ++ [m]
      curMoves = take p moves

checkWon :: Moves -> Maybe Player
checkWon mvs = case (isWin pX, isWin pO) of
  (True, _) -> Just Player_X
  (False, True) -> Just Player_O
  (False, False) -> Nothing
  where
    -- find if any set of winning moves are present in player' moves
    isWin pmvs = any (\wmvs -> all (\wm -> elem wm pmvs) wmvs) winningPos
    -- divide moves for X and O
    (pX, pO) = f mvs Player_X ([], [])
    f [] _ v = v
    f (m:ms) Player_X (xs, os)= f ms Player_O (xs ++ [m], os)
    f (m:ms) Player_O (xs, os)= f ms Player_X (xs, os ++ [m])

gameBoard
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => Dynamic t Squares
  -> m (Event t Action)
gameBoard squares = divClass "game-board" $ do
  evs <- forM [minBound .. maxBound] $ \row -> divClass "board-row" $ do
    evs <- forM [minBound .. maxBound] $ \col -> do
      let square = (row, col)
      (btn1,_) <- elAttr' "button" ("class" =: "square") $
        dynText (getSquareText square <$> squares)
      return (square <$ domEvent Click btn1)
    return (leftmost evs)
  return $ Action_DoMove <$> leftmost evs

-- | What to display on square
getSquareText :: (Row, Col) -> Squares -> Text
getSquareText square squares = case squares A.! square of
  Nothing -> "_"
  (Just Player_X) -> "X"
  (Just Player_O) -> "O"

gameInfo
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     , MonadHold t m
     )
  => Dynamic t State
  -> m (Event t Action)
gameInfo stateDyn = divClass "game-info" $ do
  let
    nextPlayer = ffor stateDyn $ \(State p _) -> if even p
      then Player_X
      else Player_O
    moveCount = (length . state_moves <$> stateDyn)

  el "div" $ do
    dyn_ $ ffor (checkWon . state_moves <$> stateDyn) $ \case
      Nothing -> do
        text "Next player: "
        dynText $ ffor nextPlayer $ \case
          Player_X -> "X"
          Player_O -> "O"
      (Just p) -> do
        text "Winner player: "
        text $ case p of
          Player_X -> "X"
          Player_O -> "O"

  el "ol" $ do
    ev1 <- el "li" $ do
      button "Go to game start"

    let
      moves = ffor moveCount $ \i -> [1..i]
    evDyn <- simpleList moves $ \iDyn -> do
      i <- sample $ current iDyn
      el "li" $ do
        ev <- button $ "Go to move #" <> T.pack (show i)
        return (Action_GoToMove i <$ ev)

    let ev2 = switch (current $ leftmost <$> evDyn)
    return $ leftmost $ [Action_GoToMove 0 <$ ev1, ev2]
