module RunGame where

import Prelude
import Test.QuickCheck.Gen (Gen)
import Effect (Effect)
-- import Effect.Console (log)
import GameModel (Model, initGame)
import GetInput (AllInput, getAllInput)
import Render (render)
import Signal (runSignal)
import SignalM (foldpR)

gameStep :: AllInput -> Model -> Gen Model
gameStep input model =
  pure
    model
      { gameStepNumber = model.gameStepNumber + 1
      , inputKey = input.key
      , gameTime = input.time
      , wsBuffer = input.wsBuffer
      }

runGame :: Effect Unit
runGame = do --onDOMContentLoaded
  initialGameModel <- initGame
  render initialGameModel
  -- create the signals
  inputSignal <- getAllInput
  -- need to be in effect monad in order to get a keyboard signal
  game <- foldpR gameStep initialGameModel inputSignal -- TODO Try Signal <> Signal
  runSignal (render <$> game)

-- runSignal ((\s -> log (show s)) <$> getJoinedInput)
