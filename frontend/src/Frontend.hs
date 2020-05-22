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
import Obelisk.Route.Frontend
import Obelisk.Generated.Static

import Reflex.Dom
import Reflex.Network

import Common.Route

import System.Random

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import qualified Data.Text as T
import Data.Bool
import Data.Bits
import Data.Time

type GameState = (Board,Player)
type Board = [Integer]
type Move = (Integer,Integer)
data Player = User | DrNim deriving Eq

other :: Player -> Player
other User = DrNim
other DrNim = User

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "FUPR Project"
      elAttr "link" ("href" =: static @"main.css"
                     <> "type" =: "text/css"
                     <> "rel" =: "stylesheet") blank

  -- prerender_ allows us to run all the code just on the client
  --            In this case we are running just (return ()) on the server
  --            and the rest of the code on the client.
  , _frontend_body = prerender_ (return ()) $ do

      -- Buttons at the top of the page
      (elMonadBtn, _) <- el' "button" $ text "MonadClicker"
      (elNimBtn, _)   <- el' "button" $ text "Nim"

      -- Get click events when the nimButtons at the top are clicked
      let eMonadClick = domEvent Click elMonadBtn
      let eNimClick   = domEvent Click elNimBtn

      -- Deliver the appropriate game as a "widget" when each button is clicked
      -- where "monadClickerWidget" is the default
      widgetHold_ monadClickerWidget $ leftmost [ monadClickerWidget <$ eMonadClick
                                                , nimWidget          <$ eNimClick
                                                ]
      return ()
  }

purchaseButton :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t Bool
  -> Dynamic t Integer
  -> m ( Element EventResult (DomBuilderSpace m) t)
purchaseButton dCanAfford dValue = do
  rec
    (elBtn, _) <- elDynAttr' "button" buttonAttrs $ dynText (tshow <$> dValue)

    let buttonClassDelegator = \canAfford isHover -> ("class" =: bool "button disabled" (bool "button" "button hover" isHover) canAfford)
    let buttonAttrs = zipDynWith buttonClassDelegator dCanAfford dHover

    let eMouseOver = domEvent Mouseover elBtn
    let eMouseOut  = domEvent Mouseout  elBtn

    dHover <- holdDyn False $ leftmost [True <$ eMouseOver, False <$ eMouseOut]
  return elBtn

functorConstructor :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Show b)
  => Integer
  -> Dynamic t Integer
  -> Event t Integer
  -> Integer
  -> Event t a
  -> b
  -> m (Dynamic t Integer, Event t (Integer -> Integer))
functorConstructor metaness dNumMetaFunctors eMonadsUpdate functorPrice eTick tickerHz = do
  rec
    dCanAffordSpam <- holdDyn False ((\(a,b) -> a <= b) <$> (attachPromptlyDyn dFunctorPrice eMonadsUpdate))
    dCanAfford <- holdUniqDyn dCanAffordSpam
    let eCanAfford = updated dCanAfford
    eFunctorPurchase  <- switchHold never $
                         leftmost [ eFunctorButtonClick <$ ffilter id eCanAfford
                                  , never               <$ ffilter not eCanAfford
                                  ]

    dNumFunctors <- foldDyn ($) 0 . mergeWith (.) $
                    [ (+1) <$ eFunctorPurchase
                    , (\x y -> x + y) <$> (tagPromptlyDyn dNumMetaFunctors eTick)
                    ]

    let fFunctorPrice = (\n -> functorPrice * (n+1)^2)

    let dFunctorPrice = ffor dNumFunctors fFunctorPrice

    --  eFunctorCost evaluates dNumFunctors as having already purchased the item, so we need to subtract one.
    let eFunctorCost  = (\x y -> y - (fFunctorPrice (x - 1))) <$> (tagPromptlyDyn dNumFunctors eFunctorPurchase)

    let name = if metaness > 0 then
          "Meta" ++ (if metaness > 1 then "^" ++ (show metaness) else "") ++ " Functor"
          else "Functor"

    elFunctorButton <- el "div" $ do
      elBtn <- purchaseButton dCanAfford dNumFunctors
      el "span" $ text $ T.pack $ name ++ " => (+" ++ (show tickerHz) ++ " Ms/s) : "
      el "span" $ dynText $ (fmap tshow (dFunctorPrice))
      el "span" $ text " Ms \t"
      return elBtn

    let eFunctorButtonClick = domEvent Click elFunctorButton

  return (dNumFunctors, eFunctorCost)


metaFunctorButtons :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Show b)
  => Integer
  -> Event t Integer
  -> Event t a -> b
  -> (Integer -> Integer)
  -> m (Dynamic t Integer, [Event t (Integer -> Integer)])
metaFunctorButtons metaness eMonadsUpdate eTick tickerHz priceOf = inner 0
  where inner n = do
          rec
            (dNum, eCost) <- (functorConstructor
                             n
                             (dNumMetaer)
                             eMonadsUpdate
                             (priceOf n)
                             eTick
                             tickerHz)
            (dNumMetaer, eCosts) <- (if n == metaness
                                    then return (constDyn 0, [])
                                    else inner (n+1))
          return (dNum, (eCost:eCosts))



clickFunctionUpgrades :: (
  DomBuilder t m,
  PostBuild t m,
  MonadHold t m,
  MonadFix m
  )
  => Integer
  -> Dynamic t (Integer -> Integer)
  -> Event t Integer
  -> m (Dynamic t (Integer -> Integer), [Event t (Integer -> Integer)])
clickFunctionUpgrades n dInitialFunc eMonadsUpdate = inner 0 dInitialFunc []
      where
    inner n' dFunc costEvents
      | n  <  0    = do return (dFunc, costEvents)
      | n' >= n    = do return (dFunc, costEvents)
      | otherwise = do
        rec
          elBtn <- el "div" $ do
            elBtn <- purchaseButton dCanAfford ((\x -> x-1) <$> dNumComposers)
            el "span" $ text $ T.pack $ "Level " ++ (show n') ++ " upgrades :: "
            el "span" $ dynText $ (fmap tshow (dComposerPrice))
            el "span" $ text " Monads"
            return elBtn

          let eBtnClick = domEvent Click elBtn

          -- NOTE: This is a repeated pattern we might be able to abstract
          --       or maybe find a function that does this more elegantly
          dCanAffordSpam <- holdDyn False ((\(a,b) -> a <= b) <$> (attachPromptlyDyn dComposerPrice eMonadsUpdate))
          dCanAfford <- holdUniqDyn dCanAffordSpam
          let eCanAfford = updated dCanAfford

          eComposerPurchase  <- switchHold never $
                         leftmost [ eBtnClick <$ ffilter id  eCanAfford
                                  , never     <$ ffilter not eCanAfford
                                  ]

          -- NOTE: This should be parameterized as an argument to the top-level function
          let fComposerPrice y = 6^(2 * n' + 1) * 2 ^ (y-1)

          let dComposerPrice = ffor dNumComposers (fComposerPrice)

          let eComposerCost = (\x y -> y - fComposerPrice (x-1)) <$> (tagPromptlyDyn dNumComposers eComposerPurchase)

          dNumComposers <- (+1) <$> (count eComposerPurchase)

          let itercomp n f = if n <= 0 then id else f . (itercomp (n-1) f)

          -- Very complex way to get multiplication currently :)
          let dComposeLevel = zipDynWith (\f g -> g f) dFunc (itercomp <$> dNumComposers)

          retFunc <- inner (n'+1) dComposeLevel (eComposerCost:costEvents)

        return retFunc


monadClickerWidget ::(
  PerformEvent t m,
  TriggerEvent t m,
  MonadIO m,
  MonadIO (Performable m),
  MonadHold t m,
  MonadFix m,
  DomBuilder t m ,
  Routed t (R FrontendRoute) m ,
  PostBuild t m
  ) => m ()
monadClickerWidget = do
      let functorPrice         = 256
      let multiplier           = 64 -- Should be a function, the exponential growth overshadows this
      let tickerHz             = 2
      rec
        el "h1" $ text "Monadclicker"
        el "p"  $ text "Click the monad, buy more advanced concepts to become the endofunctorial master."

        now   <- liftIO getCurrentTime
        eTick <- tickLossy (1 / tickerHz) now

        -- MONAD BUTTON
        dButtonText <- holdDyn ("Get Monads") $ (\n -> T.pack (show n ++ " Monads")) <$> eMonadsUpdate

        (elMonadBtn, _) <- elAttr' "button" ("class" =: "button big") $ dynText dButtonText
        let eMonadBtnClick = domEvent Click elMonadBtn

        -- For debugging
        --(elResetBtn, _ ) <- elAttr' "button" ("class" =: "button") $ text "RESET MONADS"
        --let eResetBtnClick = domEvent Click elResetBtn

        -- CLICK FUNCTION UPGRADES
        el "h2" $ text "Upgrade click function"

        (dClickFunc, clickUpgradeCostEvents) <- clickFunctionUpgrades 5 (constDyn succ) eMonadsUpdate

        -- FUNCTORLAND
        el "h2" $ text "Functors"

        let priceOf metaness = functorPrice * multiplier ^ metaness
        (dNumFunctors, functorCostEvents) <- metaFunctorButtons 18 eMonadsUpdate eTick tickerHz priceOf

        -- OTHER THINGS
        let eMonadsUpdate = updated dMonads

        let costEvents = functorCostEvents ++ clickUpgradeCostEvents

        dMonads <- foldDyn ($) 0 . mergeWith (.) $
          costEvents ++ 
          [ tagPromptlyDyn dClickFunc eMonadBtnClick
          , (\funcs val -> val + funcs) <$> ffilter (>0) (tagPromptlyDyn dNumFunctors eTick)
          ]

      return ()


--
----
------
--------
----------
-- BEGIN NIM
----------
--------
------
----
--

nimWidget :: (
  PerformEvent t m,
  TriggerEvent t m,
  MonadIO m,
  MonadIO (Performable m),
  MonadHold t m,
  MonadFix m,
  DomBuilder t m ,
  Routed t (R FrontendRoute) m ,
  PostBuild t m
  ) => m ()
nimWidget = do

      el "h1"  $ text "Welcome to Nim"

      el "div" $ do
          el "p" $ text "Try to beat Dr. Nim, the master of all nimians."
          el "p" $ text "Win by taking the last bead."

      let buttonAttrs = ("class" =: "button")
      rec
        startButtonAttrs <- holdDyn buttonAttrs $ leftmost
          [ ("class" =: "button removed") <$ eBtn
          , buttonAttrs <$ eGameOver
          ]
        (elBtn, _) <- do
          (innerElBtn,_) <- elDynAttr' "button" startButtonAttrs $ text "Start"
          return $ (innerElBtn,1)

        let eBtn = domEvent Click elBtn

        now <- liftIO getCurrentTime
        eTick <- tickLossy 1 now
        -- dClock <- clockLossy 0.3 now
        rng <- liftIO newStdGen

        let debug = False

        gameText <-
          foldDyn ($) "" . mergeWith (.) $ [
              (\_ -> "") <$ eBtn
            , (\(_, p) -> \_ -> "Game Over, You " ++ if p == User then "lost" else "won!") <$> eGameOver
          ]

        el "h1" $ dynText $ fmap T.pack gameText

        let eStateUpdate = updated dState
        let eGameOver    = ffilter (emptyBoard . fst) eStateUpdate
        let ePlayerMoved = ffilter (\p -> DrNim == snd p) eStateUpdate
        let eDrNimMoved  = ffilter (\p -> User == snd p) eStateUpdate

        eDrNimMove <-
             switchHold never $ leftmost [ never <$ eGameOver
                                         , eTick <$ ePlayerMoved
                                         , never <$ eStateUpdate
                                         ]
        ePlayerMove <-
             switchHold never $ leftmost [ eBeadClick <$ eDrNimMoved
                                         , never <$ eStateUpdate
                                         ]


        dState :: Dynamic t GameState <-
          foldDyn ($) ([], User) . mergeWith (.)
          $ [ fmap (\m (b,p) -> (nim m b, other p)) ePlayerMove
            , fmap (\_ (b,p) -> (nim (bestMove b rng) b, other p)) eDrNimMove
            , (\_ -> (reverse [1..5], User)) <$ eBtn
            ]

        huh :: Event t (Event t Move, Event t Move) <- networkView $ nimButtons <$> dState
        eBeadHover :: Event t Move <- switchHold never $ fst <$> huh
        eBeadClick :: Event t Move <- switchHold never $ snd <$> huh

        -- if debug then do
        --   bom <- holdDyn (0,0) $ eBeadClick
        --   bim <- holdDyn (0,0) $ eBeadHover
        --   el "h1" $ dynText $ fmap tshow $ bom
        --   el "p" $ dynText $ fmap tshow $ bim
        -- else do return ()
      return ()

nimButtons ::
  ( DomBuilder t m
  , PostBuild t m
  , MonadHold t m
  , MonadFix m) => GameState -> m (Event t Move, Event t Move)
nimButtons (nimState,player) = do
    events <- forM (zip [0..] nimState) $ \(pile, pileSize) -> divClass "row" $ do
      let lamb = (
            \(accHover, accOut, accClick) bead -> do
              let move = (pile, bead)
              let buttonAttrs = ("class" =: "button")
              rec
                let hoverAttrs = ("class" =: "button hover") <$ (leftmost [eBeadHover, () <$ accHover])
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


bestMove :: Board -> StdGen -> Move
bestMove board rng = case goodMoves of
                      [] -> moves !! (fst $ randomR (0, length moves - 1) rng)
                      (x:_) -> x
  where moves = getAllMoves board
        goodMoves = filter (\x -> (foldr xor 0 (nim x board))==0) moves

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

both :: (a -> b) -> (a,a) -> (b,b)
both f (a,b) = (f a, f b)

