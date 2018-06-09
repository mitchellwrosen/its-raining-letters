{-# language LambdaCase          #-}
{-# language OverloadedStrings   #-}
{-# language PatternSynonyms     #-}
{-# language RecursiveDo         #-}
{-# language ScopedTypeVariables #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Function
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.Sequence (Seq, pattern (:|>))
import Data.Tuple (swap)
import Graphics.Vty (Image, Key(..), Picture, Vty)
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Random.MWC

import qualified Data.ByteString.Char8 as Latin1
import qualified Data.HashMap.Strict as HashMap
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
moment gen (cw, ch) eKey = mdo
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
    threadDelay 600000

  let eCorrectChar :: Event Char
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
  let eIncorrectChar :: Event Char
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

  let eNextLevel :: Event ()
      eNextLevel =
        filterJust
          ((\score -> guard (score > 0 && score `rem` 40 == 0))
            <$> bScore
            <@ eCorrectChar)

  bLevels :: Behavior [ByteString] <- do
    accumB levels (tail <$ eNextLevel)

  bLetters :: Behavior (Seq (Loc Char)) <-
    bLettersGen gen (cw, ch) eTick eCorrectChar (head <$> bLevels)

  bScore :: Behavior Int <-
    accumB 0
      (unions
        [ (+1) <$ eCorrectChar
        , subtract 1 <$ eIncorrectChar
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

  let eWin :: Event ()
      eWin =
        filterJust
          ((\case
            [_] ->
              Just ()
            _ ->
              Nothing)
          <$> bLevels <@ eNextLevel)

  let render :: [Loc Char] -> Int -> [Image]
      render letters score =
        Vty.string Vty.defAttr ("Score: " <> show score) :
          map
            (\(Loc c r v) ->
              Vty.translate c r
                (Vty.char Vty.defAttr (qwertyToColemak HashMap.! v)))
            letters

  let bPicture0 :: Behavior Picture
      bPicture0 =
        (\letters score ->
          Vty.picForLayers (render (toList letters) score))
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

qwertyToColemak :: HashMap Char Char
qwertyToColemak =
  HashMap.fromList
    (zip
      "abcdefghijklmnopqrstuvwxyz;,./"
      "abcsftdhuneimky;qprglvwxjzo,./")

colemakToQwerty :: HashMap Char Char
colemakToQwerty =
  (HashMap.fromList . map swap . HashMap.toList) qwertyToColemak

randomChar :: GenIO -> ByteString -> IO Char
randomChar gen bytes =
  Latin1.index bytes <$>
    uniformR (0, Latin1.length bytes - 1) gen

data Loc a
  = Loc !Int !Int a

(&>) :: Functor f => f a -> b -> f b
(&>) = flip (<$)
