module DotenvParser where

import Fetch
import Prelude

import Effect (Effect)
import Effect.Aff (Aff,runAff)
import Effect.Console (log)
import Control.Monad.Except (runExceptT, throwError)
import Effect.Exception (error)
import Data.Either (Either(..))
-- import Data.Function (const)

-- fetchFile :: String -> Effect String
-- fetchFile url = do 
--     -- response <- Fetch.get url defaultFetchOptions 
--     responseText <- Fetch.text url -- response 
--     pure responseText

-- fetchFile :: String -> Effect String
fetchFile ∷ String → Aff String
fetchFile requestUrl = do 
        -- let requestUrl = "https://httpbin.org/get"
        { status, text } <- fetch requestUrl { headers: { "Accept": "application/json" }}
        responseBody <- text
        pure responseBody
        -- pure unit


fectFileEffect :: String -> Effect String
fectFileEffect url = --runAff (throwError <<< error) pure (fetchFile url)
  runExceptT $ runAff process (fetchFile url)

  where process result = 
            case result of
                Left err -> do
                -- Обрабатываем ошибку (если нужно, можно заменить error на что-то более подходящее)
                    throwError $ error $ "Error fetching file: " <> show err
                Right content -> do
                -- Возвращаем содержимое файла
                    pure content


-- runAff (const identity) (const pure) (fetchFile url)