module Engine.ResourceLoader
  ( getHtmlElement
  , loadImages
  , loadJson
  , parseConfigFile
  )
  where

import Engine.Reexport
import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web (driver)
import Control.Bind (join)
import Data.Argonaut.Parser (jsonParser)
import Data.HTTP.Method (Method(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Nullable (Nullable, toMaybe)
import Effect.Exception (Error, error)
import Engine.Config (Config, fromJson)
import Engine.Types (FilePath)
import Graphics.Canvas (CanvasImageSource, tryLoadImage)

foreign import _getHtmlElenentById :: String -> Effect (Nullable HTMLElement)

fileLoader :: FilePath -> Aff (Maybe String)
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

loadJson :: FilePath -> Aff (Either String Json)
loadJson filePath = do
  mbFile <- fileLoader filePath
  case mbFile of
    Nothing -> pure $ Left ("ERROR: File not found: " <> filePath)
    Just file -> pure $ mapLeft 
                          (\err -> "Invalid json structure in config file: \n" <> filePath <> "\n" <> err) 
                          (jsonParser file)

parseConfigFile ::  
  FilePath -> 
  Aff (Either String Config)
parseConfigFile configFilePath = do 
  eitherJson <- loadJson configFilePath
  pure $ join (fromJson <$> eitherJson)
  -- join <$> ((map fromJson) <$> loadJson configFilePath) 

tryLoadImageAff :: FilePath -> Aff CanvasImageSource
tryLoadImageAff path = makeAff wrappedFn
  where
  wrappedFn :: (Either Error CanvasImageSource -> Effect Unit) -> Effect Canceler
  wrappedFn done = do
    tryLoadImage path
      ( \maybeImage -> case maybeImage of
          Just canvasImage -> done (Right canvasImage)
          Nothing -> done (Left (error $ "tryLoadImageAff Error: Could not load " <> path))
      )
    pure mempty

loadImages :: Array { name :: String, path :: String } -> Aff (Map String CanvasImageSource)
loadImages files = do
  images <- traverse (\{ name, path } -> (\img -> Tuple name img) <$> tryLoadImageAff path) files
  pure $ Map.fromFoldable images


getHtmlElement :: String -> Effect (Maybe HTMLElement)
getHtmlElement nameId = do 
  foreignElem <- _getHtmlElenentById nameId
  pure $ toMaybe foreignElem