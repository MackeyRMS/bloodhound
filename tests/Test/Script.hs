{-# LANGUAGE OverloadedStrings #-}

module Test.Script where

import qualified Data.Aeson.KeyMap as X
import qualified Data.Map as M
import Test.Common
import Test.Import

spec :: Spec
spec =
  describe "Script" $
    it "returns a transformed document based on the script field" $
      withTestEnv $ do
        _ <- insertData
        let query = MatchAllQuery Nothing
            sfv =
              toJSON $
                Script
                  (Just (ScriptLanguage "painless"))
                  (ScriptInline "doc['age'].value * 2")
                  Nothing
            sf =
              ScriptFields $
                X.fromList [("test1", sfv)]
            search' = mkSearch (Just query) Nothing
            search = search' {scriptFields = Just sf}
        resp <- searchByIndex testIndex search
        parsed <- parseEsResponse resp :: BH IO (Either EsError (SearchResult Value))
        case parsed of
          Left e ->
            liftIO $ expectationFailure ("Expected a script-transformed result but got: " <> show e)
          Right sr -> do
            let Just results =
                  hitFields (head (hits (searchHits sr)))
            liftIO $
              results `shouldBe` HitFields (M.fromList [("test1", [Number 20000.0])])
