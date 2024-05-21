module ResourceLoader where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Canceler, Aff, makeAff)
import Effect.Exception (Error, error)
import Graphics.Canvas (CanvasImageSource, tryLoadImage)
import Affjax as AX
import Affjax.Web (driver)
import Affjax.ResponseFormat as ResponseFormat
import Data.HTTP.Method (Method(..))

type Path
  = String

fileLoader :: Path -> Aff (Maybe String)
fileLoader resource = do
  res <- AX.request driver settings
  case res of
    Right response -> pure (Just response.body)
    Left err -> pure Nothing
  where
  settings =
    ( AX.defaultRequest
        { url = resource -- (show resource)
        , method = Left GET
        , responseFormat = ResponseFormat.string
        }
    )

tryLoadImageAff :: String -> Aff CanvasImageSource
tryLoadImageAff path = makeAff wrappedFn
  where
  wrappedFn :: (Either Error CanvasImageSource -> Effect Unit) -> Effect Canceler
  wrappedFn done = do
    tryLoadImage path
      ( \maybeImage -> case maybeImage of
          Just canvasImage -> done (Right canvasImage)
          Nothing -> done (Left (error $ "Could not load " <> path))
      )
    pure mempty
