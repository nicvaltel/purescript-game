module RunGame where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import GameModel (Model, initGame)
import GetInput (AllInput, SignalType(..), SignalType, getAllInput)
import Render (render)
import Signal (runSignal)
import SignalM (foldpR)
import Test.QuickCheck.Gen (Gen)
import Data.Array ((:))

gameStep :: SignalType -> Model -> Gen Model
gameStep (UserInputSignal userInput) model =
  pure
    model
      { gameStepNumber = model.gameStepNumber + 1
      , inputKey = userInput.key
      }
gameStep (WebSocketSignal ws) model = 
  pure
    model
      { gameStepNumber = model.gameStepNumber + 1
      , wsBuffer = ws : model.wsBuffer
      }
gameStep (FrameRateSignal time) model = 
  pure
    model
      { gameStepNumber = model.gameStepNumber + 1
      , gameTime = time
      , wsBuffer = []
      }

runGame :: Effect Unit
runGame = do --onDOMContentLoaded
  initialGameModel <- initGame
  render initialGameModel
  -- create the signals
  inputSignal <- getAllInput

  -- runSignal (log <<< show <$> inputSignal)
  -- need to be in effect monad in order to get a keyboard signal
  game <- foldpR gameStep initialGameModel inputSignal -- TODO Try Signal <> Signal
  runSignal (render <$> game)

-- runSignal ((\s -> log (show s)) <$> getJoinedInput)
