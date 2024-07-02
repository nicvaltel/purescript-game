module Engine.WebSocket.WSSignalChan
  ( WSMessage
  , WSocket(..)
  , exampleOfUsageWSSignalChan
  , initWSSignal
  , initWebSocket
  , onClose
  , onMessage
  , onOpen
  , sendMessage
  , webSocketConnectionStatusIsOpen
  )
  where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Signal (Signal, runSignal)
import Signal.Channel as SC

data WSocket

type WSURL
  = String

type WSMessage
  = String

foreign import _wsocket :: String -> Effect WSocket

foreign import _addEventListenerConnectionIsOpen :: WSocket -> Effect Unit

foreign import _addEventListenerConnectionIsClose :: WSocket -> Effect Unit

foreign import _addEventListenerMessageRecieved :: forall a. WSocket -> (String -> Effect a) -> Effect a

foreign import _sendMessage :: WSocket -> String -> Effect Unit

foreign import _webSocketConnectionStatusIsOpen :: WSocket -> Effect Boolean

sendMessage ∷ WSocket → String → Effect Unit
sendMessage = _sendMessage

initWebSocket :: WSURL -> Effect WSocket
initWebSocket = _wsocket

onOpen :: WSocket -> Effect Unit
onOpen = _addEventListenerConnectionIsOpen

onClose :: WSocket -> Effect Unit
onClose = _addEventListenerConnectionIsClose

onMessage :: forall a. WSocket -> (String -> Effect a) -> Effect a
onMessage = _addEventListenerMessageRecieved

webSocketConnectionStatusIsOpen :: WSocket -> Effect Boolean
webSocketConnectionStatusIsOpen = _webSocketConnectionStatusIsOpen

addListenerWSMessageToSignal :: WSocket -> Effect (Signal String)
addListenerWSMessageToSignal socket = do
  chan <- SC.channel ""
  _addEventListenerMessageRecieved socket (\msg -> SC.send chan msg)
  pure $ SC.subscribe chan

initWSSignal :: WSURL -> Effect (Signal String)
initWSSignal url = do
  socket <- _wsocket url
  _addEventListenerConnectionIsOpen socket
  _addEventListenerConnectionIsClose socket
  addListenerWSMessageToSignal socket

exampleOfUsageWSSignalChan :: Effect Unit
exampleOfUsageWSSignalChan = do
  messageSignal <- initWSSignal "ws://95.140.155.123:1234/ws"
  runSignal (map renderTickAction messageSignal)
  pure unit
  where
  renderTickAction :: forall a. Show a => a -> Effect Unit
  renderTickAction a = log $ "RECIEVED :" <> show a
