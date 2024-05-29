module Engine.Utils.SignalM where

import Prelude
import Data.Array ((:), take, null, unsafeIndex)
import Data.Tuple (Tuple(..), fst, uncurry)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Signal (Signal, foldp)
import Signal.Time (every)
import Test.QuickCheck (randomSeed)
import Test.QuickCheck.Gen (Gen, runGen, evalGen, GenState)

--runState :: forall s a. State s a -> s -> Tuple a s
foldpM :: forall a b mb c. (mb -> c -> Tuple b c) -> c -> (a -> b -> mb) -> b -> (Signal a) -> (Signal b)
foldpM run st' f st sig = map fst $ foldp (\xa (Tuple xb xc) -> uncurry run (Tuple (f xa xb) xc)) (Tuple st st') sig

foldpR' :: forall a b. GenState -> (a -> b -> Gen b) -> b -> (Signal a) -> (Signal b)
foldpR' = foldpM runGen

-- $ runState . unGen
foldpR :: forall a b e. (a -> b -> Gen b) -> b -> (Signal a) -> Effect (Signal b)
foldpR f st sig = do
  seed <- randomSeed
  pure $ foldpR' { newSeed: seed, size: 536870911 } f st sig

evalGenD :: forall a. Gen a -> Effect a
evalGenD g = do
  seed <- randomSeed
  pure $ evalGen g { newSeed: seed, size: 536870911 }

frameRateQuick :: Signal Number
frameRateQuick = every 100.0

type BufferLength
  = Int

joinSignals :: forall a. BufferLength -> Signal a -> Signal (Array (Tuple Int a))
joinSignals bufferLen sig = foldp accumulate [] sig
  where
  accumulate a arr
    | null arr = [ Tuple 0 a ]
    | otherwise = let (Tuple n _) = unsafePartial (arr `unsafeIndex` 0) in take bufferLen $ (Tuple (n + 1) a) : arr

-- getJoinedInput = sampleOn frameRate (joinSignals 32 frameRateQuick)
