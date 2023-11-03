{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Bloodhound.Internal.Source where

import           Data.Aeson
import           Data.Text
import           GHC.Generics


data Source
  = NoSource
  | SourcePatterns PatternOrPatterns
  | SourceIncludeExclude Include Exclude
  deriving (Eq, Show, Generic)

instance ToJSON Source where
  toJSON NoSource = toJSON False
  toJSON (SourcePatterns patterns) = toJSON patterns
  toJSON (SourceIncludeExclude incl excl) = object ["includes" .= incl, "excludes" .= excl]

data PatternOrPatterns
  = PopPattern Pattern
  | PopPatterns [Pattern]
  deriving (Eq, Read, Show, Generic)

instance ToJSON PatternOrPatterns where
  toJSON (PopPattern pattern)   = toJSON pattern
  toJSON (PopPatterns patterns) = toJSON patterns

newtype Include = Include [Pattern] deriving (Eq, Read, Show, Generic)

newtype Exclude = Exclude [Pattern] deriving (Eq, Read, Show, Generic)

instance ToJSON Include where
  toJSON (Include patterns) = toJSON patterns

instance ToJSON Exclude where
  toJSON (Exclude patterns) = toJSON patterns

newtype Pattern = Pattern Text deriving (Eq, Read, Show, Generic)

instance ToJSON Pattern where
  toJSON (Pattern pattern) = toJSON pattern
