{-# language LambdaCase          #-}
{-# language PatternSynonyms     #-}
{-# language RecursiveDo         #-}
{-# language ScopedTypeVariables #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Sequence (Seq, pattern (:|>))
import Graphics.Vty (Image, Key(..), Picture, Vty)
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Random.MWC

import qualified Data.Sequence as Seq
import qualified Graphics.Vty as Vty

main :: IO ()
main =
  bracket
    (Vty.mkVty =<< Vty.standardIOConfig)
    Vty.shutdown
    main'

main' :: Vty -> IO ()
main' vty = do
  gen :: GenIO <-
    createSystemRandom

  (keyAddHandler, fireKey) <-
    newAddHandler

  (cw, ch) <-
    Vty.displayBounds (Vty.outputIface vty)

  doneVar :: TMVar () <-
    newEmptyTMVarIO

  network :: EventNetwork <-
    compile $ do
      eKey :: Event Key <-
        fromAddHandler keyAddHandler

      bPicture :: Behavior (Maybe Picture) <-
        moment gen (cw, ch) eKey

      ePicture :: Event (Future (Maybe Picture)) <-
        changes bPicture

      let handle :: Maybe Picture -> IO ()
          handle = \case
            Nothing ->
              atomically (putTMVar doneVar ())
            Just picture ->
              Vty.update vty picture

      liftIO . handle =<< valueB bPicture

      reactimate' ((fmap.fmap) handle ePicture)

  actuate network

  fix $ \loop ->
    join . atomically $
      (do
        readTChan (Vty._eventChannel (Vty.inputIface vty)) >>= \case
          Vty.EvKey KEsc _ ->
            pure (pure ())
          Vty.EvKey key _ ->
            pure $ do
              fireKey key
              loop
          Vty.EvResize _ _ ->
            pure (pure ())
          _ ->
            pure loop)
      <|>
      (do
        takeTMVar doneVar
        pure (pure ()))

moment
  :: GenIO
  -> (Int, Int)
  -> Event Key
  -> MomentIO (Behavior (Maybe Picture))
moment gen (_, ch) eKey = mdo
  let eChar :: Event Char
      eChar =
        filterJust
          ((\case
            KChar c ->
              Just c
            _ ->
              Nothing)
          <$> eKey)

  (eTick, fireTick) <-
    newEvent

  liftIO . void . forkIO . forever $ do
    fireTick ()
    threadDelay 300000

  bLetters :: Behavior (Seq (Loc Char)) <- do
    eNewLetter :: Event (Loc Char) <-
      execute
        (eTick &>
          liftIO
            (Loc
              <$> uniformR (0, 4) gen
              <*> pure 0
              <*> (chr <$> uniformR (ord 'a', ord 'z') gen)))

    accumB mempty
      (unions
        [ (Seq.<|) <$> eNewLetter
        , eTick &>
            ((\f -> fmap fromJust . Seq.filter isJust . fmap f)
              (\(Loc c r v) -> do
                guard (r < ch-1) :: Maybe ()
                pure (Loc c (r+1) v)))
        , (\c cs ->
            case Seq.findIndexR (\(Loc _ _ c') -> c == c') cs of
              Nothing ->
                cs
              Just i ->
                Seq.deleteAt i cs)
          <$> eChar
        ])

  let eLose :: Event ()
      eLose =
        filterJust (f <$> bLetters <@ eTick)
       where
        f :: Seq (Loc Char) -> Maybe ()
        f = \case
          _ :|> Loc _ h _ | h == ch-1 ->
            pure ()
          _ ->
            Nothing

  let render :: [Loc Char] -> [Image]
      render = \case
        [] ->
          [Vty.emptyImage]
        cs ->
          map (\(Loc c r v) -> Vty.translate c r (Vty.char Vty.defAttr v)) cs

  let bPicture0 :: Behavior Picture
      bPicture0 =
        Vty.picForLayers . render . toList <$> bLetters

  switchB (Just <$> bPicture0) (pure Nothing <$ eLose)

data Loc a
  = Loc !Int !Int a

(&>) :: Functor f => f a -> b -> f b
(&>) = flip (<$)
