{-# language LambdaCase          #-}
{-# language OverloadedStrings   #-}
{-# language PatternSynonyms     #-}
{-# language RankNTypes          #-}
{-# language RecursiveDo         #-}
{-# language ScopedTypeVariables #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens (Prism', preview, prism')
import Control.Monad
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Function
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.Sequence (Seq, pattern (:|>))
import Data.Tuple (swap)
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Random.MWC

import qualified Data.ByteString.Char8 as Latin1
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence as Seq
import qualified Termbox

main :: IO ()
main =
  Termbox.main main'

main' :: IO ()
main' = do
  gen :: GenIO <-
    createSystemRandom

  (keyAddHandler, fireKey) <-
    newAddHandler

  (cw, ch) <-
    Termbox.size

  doneVar :: TMVar () <-
    newEmptyTMVarIO

  network :: EventNetwork <-
    compile $ do
      eKey :: Event Termbox.Key <-
        fromAddHandler keyAddHandler

      bPicture :: Behavior (Maybe (IO ())) <-
        moment gen (cw, ch) eKey

      ePicture :: Event (Future (Maybe (IO ()))) <-
        changes bPicture

      let
        render :: Maybe (IO ()) -> IO ()
        render = \case
          Nothing ->
            atomically (putTMVar doneVar ())
          Just picture -> do
            Termbox.clear mempty mempty
            picture
            Termbox.flush

      liftIO . render =<< valueB bPicture

      reactimate' ((fmap.fmap) render ePicture)

  actuate network

  eventChan :: TChan Termbox.Event <-
    newTChanIO

  (void . forkIO . forever) $ do
    Termbox.poll >>= atomically . writeTChan eventChan

  fix $ \loop ->
    join . atomically $
      (do
        readTChan eventChan >>= \case
          Termbox.EventKey Termbox.KeyEsc _ ->
            pure (pure ())
          Termbox.EventKey key _ ->
            pure $ do
              fireKey key
              loop
          Termbox.EventResize _ _ ->
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
  -> Event Termbox.Key
  -> MomentIO (Behavior (Maybe (IO ())))
moment gen (cw, ch) eKey = mdo
  let
    eChar :: Event Char
    eChar =
      previewE _KeyChar eKey

  (eTick, fireTick) <-
    newEvent

  liftIO . void . forkIO . forever $ do
    fireTick ()
    threadDelay 600000

  let
    eCorrectChar :: Event Char
    eCorrectChar =
      filterJust
        ((\cs c ->
          case cs of
            _ :|> Loc _ _ c' | c == c' ->
              Just c
            _ ->
              Nothing)
        <$> bLetters <@> eChar)

  -- This would easier to write with better event merging functions
  let
    eIncorrectChar :: Event Char
    eIncorrectChar =
      filterJust
        ((\cs c ->
          case cs of
            _ :|> Loc _ _ c' -> do
              guard (c /= c')
              pure c
            _ ->
              Nothing)
        <$> bLetters <@> eChar)

  eNextLevel :: Event () <- do
    bLevelNum :: Behavior Int <-
      accumB 0 ((+1) <$ eNextLevel)
    pure
      (filterJust
        ((\score lvl -> do
          guard (score > 0 && score `rem` 40 == 0 && score `div` 40 > lvl))
          <$> bScore
          <*> bLevelNum
          <@ eCorrectChar))

  bLevels :: Behavior [ByteString] <- do
    accumB levels (tail <$ eNextLevel)

  bLetters :: Behavior (Seq (Loc Char)) <-
    bLettersGen gen (cw, ch) eTick eCorrectChar (head <$> bLevels)

  bScore :: Behavior Int <-
    accumB 0
      (unions
        [ (+1) <$ eCorrectChar
        , subtract 2 <$ eIncorrectChar
        ])

  let
    eLose :: Event ()
    eLose =
      filterJust (f <$> bLetters <@ eTick)
     where
      f :: Seq (Loc Char) -> Maybe ()
      f = \case
        _ :|> Loc _ h _ | h == ch-1 ->
          pure ()
        _ ->
          Nothing

  let
    eWin :: Event ()
    eWin =
      filterJust
        ((\case
          [_] ->
            Just ()
          _ ->
            Nothing)
        <$> bLevels <@ eNextLevel)

  let
    render :: [Loc Char] -> Int -> IO ()
    render letters score =
      mconcat
        [ for_
            (zip [0..] ("Score: " ++ show score))
            (\(i, v) ->
              Termbox.set i 0 (Termbox.Cell v Termbox.black Termbox.white))

        , foldMap
            (\(Loc c r v) ->
              Termbox.set c r
                (Termbox.Cell (qwertyToColemak v) mempty mempty))
            letters
        ]

  let
    bPicture0 :: Behavior (IO ())
    bPicture0 =
      (\letters score ->
        render (toList letters) score)
      <$> bLetters <*> bScore

  switchB (Just <$> bPicture0) (pure Nothing <$ (eLose <> eWin))

bLettersGen
  :: GenIO
  -> (Int, Int)
  -> Event ()
  -> Event Char
  -> Behavior ByteString
  -> MomentIO (Behavior (Seq (Loc Char)))
bLettersGen gen (_, ch) eTick eCorrectChar bLetterSupply = do
  eNewLetter :: Event (Loc Char) <-
    execute
      ((\cs ->
        liftIO
          (Loc
            <$> uniformR (0, 4) gen
            <*> pure 1
            <*> randomChar gen cs))
      <$> bLetterSupply
      <@ eTick)

  accumB mempty
    (unions
      [ (Seq.<|) <$> eNewLetter
      , eTick &>
          ((\f -> fmap fromJust . Seq.filter isJust . fmap f)
            (\(Loc c r v) -> do
              guard (r < ch-1) :: Maybe ()
              pure (Loc c (r+1) v)))
      , (\(cs :|> _) -> cs) <$ eCorrectChar
      ])

levels :: [ByteString]
levels =
  [ -- One finger, home row
    "fj"
  , "dk"
  , "sl"
  , "a;"
  , "gh"
    -- Two fingers, home row
  , "fjdk"
  , "dksl"
  , "sla;"
  , "ghfj"
    -- Three fingers, home row
  , "fjdksl"
  , "dksla;"
  , "ghfjdk"
    -- Entire home row
  , "fjdksla;gh"
    -- One finger, top row
  , "ru"
  , "ei"
  , "wo"
  , "qp"
  , "ty"
    -- Two fingers, top row
  , "ruei"
  , "eiwo"
  , "woqp"
  , "tyru"
    -- Three fingers, top row
  , "rueiwo"
  , "eiwoqp"
  , "tyruei"
    -- Entire top row
  , "rueiwoqpty"
    -- One finger, bottom row
  , "vm"
  , "c,"
  , "x."
  , "z/"
  , "bn"
    -- Two fingers, bottom row
  , "vmc,"
  , "c,x."
  , "x.z/"
  , "bnvm"
    -- Three fingers, bottom row
  , "vmc,x."
  , "c,x.z/"
  , "bnvmc,"
    -- Entire bottom row
  , "vmc,x.z/bn"
    -- Upper area, 2x1
  , "rufj"
  , "eidk"
  , "wosl"
  , "qpa;"
  , "tygh"
    -- Upper area, 2x2
  , "eruidfjk"
  , "weiosdkl"
  , "qwopasl;"
  , "rtyufghj"
    -- Upper area, 2x3
  , "weruiosdfjkl"
  , "qweiopasdkl;"
  , "ertyuidfghjk"
    -- Upper two rows
  , "qwertyuiopasdfghjkl;"
    -- Lower area, 2x1
  , "fjvm"
  , "dkc,"
  , "slx."
  , "a;z/"
  , "ghbn"
    -- Lower area, 2x2
  , "dfjkcvm,"
  , "sdklxc,."
  , "asl;zx./"
  , "fghjvbnm"
    -- Lower area, 2x3
  , "sdfjklxcvm,."
  , "asdkl;zxc,./"
  , "dfghjkcvbnm,"
    -- Lower two rows
  , "asdfghjkl;zxcvbnm,./"
    -- Entire keyboard
  , "qwertyuiopasdfghjkl;zxcvbnm,./"
  ]

qwertyToColemak :: Char -> Char
qwertyToColemak =
  (qwertyToColemakMap HashMap.!)

qwertyToColemakMap :: HashMap Char Char
qwertyToColemakMap =
  HashMap.fromList
    (zip
      "abcdefghijklmnopqrstuvwxyz;,./"
      "abcsftdhuneimky;qprglvwxjzo,./")

colemakToQwertyMap :: HashMap Char Char
colemakToQwertyMap =
  (HashMap.fromList . map swap . HashMap.toList) qwertyToColemakMap

randomChar :: GenIO -> ByteString -> IO Char
randomChar gen bytes =
  Latin1.index bytes <$>
    uniformR (0, Latin1.length bytes - 1) gen

data Loc a
  = Loc !Int !Int a

(&>) :: Functor f => f a -> b -> f b
(&>) = flip (<$)

------------------------------------------------------------------------------
-- reactive-banana extras
------------------------------------------------------------------------------

previewE :: Prism' s a -> Event s -> Event a
previewE l =
  filterJust . fmap (preview l)

------------------------------------------------------------------------------
-- termbox extras
------------------------------------------------------------------------------

_KeyChar :: Prism' Termbox.Key Char
_KeyChar =
  prism'
    Termbox.KeyChar
    (\case
      Termbox.KeyChar c -> Just c
      _ -> Nothing)
