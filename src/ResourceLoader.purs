module ResourceLoader where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Canceler, Aff, makeAff)
import Effect.Exception (Error, error)
import Graphics.Canvas (CanvasImageSource, tryLoadImage)



tryLoadImageAff :: String -> Aff CanvasImageSource
tryLoadImageAff path = makeAff wrappedFn
  where
    wrappedFn :: (Either Error CanvasImageSource -> Effect Unit) -> Effect Canceler
    wrappedFn done = do
        tryLoadImage path (\maybeImage -> case maybeImage of
            Just canvasImage -> done (Right canvasImage)
            Nothing          -> done (Left (error $ "Could not load " <> path))
        )
        pure mempty