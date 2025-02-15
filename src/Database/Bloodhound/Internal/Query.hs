{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Database.Bloodhound.Internal.Query
  ( module X,
    module Database.Bloodhound.Internal.Query,
  )
where

import           Bloodhound.Import
import qualified Data.Aeson.KeyMap                     as X
import qualified Data.HashMap.Strict                   as HM
import qualified Data.Map.Strict                       as M
import qualified Data.Text                             as T
import           Database.Bloodhound.Common.Script     as X
import           Database.Bloodhound.Internal.Newtypes
import           Database.Bloodhound.Internal.Source
import           GHC.Generics

data Query
  = TermQuery Term (Maybe Boost)
  | TermsQuery Key (NonEmpty Text)
  | QueryMatchQuery MatchQuery
  | QueryMatchPhraseQuery MatchPhraseQuery
  | QueryMultiMatchQuery MultiMatchQuery
  | QueryBoolQuery BoolQuery
  | QueryBoostingQuery BoostingQuery
  | QueryCommonTermsQuery CommonTermsQuery
  | ConstantScoreQuery Query Boost
  | QueryFunctionScoreQuery FunctionScoreQuery
  | QueryDisMaxQuery DisMaxQuery
  | QueryFuzzyLikeThisQuery FuzzyLikeThisQuery
  | QueryFuzzyLikeFieldQuery FuzzyLikeFieldQuery
  | QueryFuzzyQuery FuzzyQuery
  | QueryHasChildQuery HasChildQuery
  | QueryHasParentQuery HasParentQuery
  | IdsQuery [DocId]
  | QueryIndicesQuery IndicesQuery
  | MatchAllQuery (Maybe Boost)
  | QueryMoreLikeThisQuery MoreLikeThisQuery
  | QueryMoreLikeThisFieldQuery MoreLikeThisFieldQuery
  | QueryNestedQuery NestedQuery
  | QueryPrefixQuery PrefixQuery
  | QueryQueryStringQuery QueryStringQuery
  | QuerySimpleQueryStringQuery SimpleQueryStringQuery
  | QueryRangeQuery RangeQuery
  | QueryRegexpQuery RegexpQuery
  | QueryExistsQuery FieldName
  | QueryMatchNoneQuery
  | QueryWildcardQuery WildcardQuery
  | QueryKnnQuery KnnQuery
  deriving (Eq, Show, Generic)

instance ToJSON Query where
  toJSON (TermQuery (Term termQueryField termQueryValue) boost) =
    object
      [ "term"
          .= object [termQueryField .= object merged]
      ]
    where
      base = ["value" .= termQueryValue]
      boosted = maybe [] (return . ("boost" .=)) boost
      merged = mappend base boosted
  toJSON (TermsQuery fieldName terms) =
    object ["terms" .= object conjoined]
    where
      conjoined = [fieldName .= terms]
  toJSON (IdsQuery docIds) =
    object ["ids" .= object conjoined]
    where
      conjoined = ["values" .= fmap toJSON docIds]
  toJSON (QueryQueryStringQuery qQueryStringQuery) =
    object ["query_string" .= qQueryStringQuery]
  toJSON (QueryMatchPhraseQuery qQueryMatchPhraseQuery) =
    object ["match_phrase" .= qQueryMatchPhraseQuery]
  toJSON (QueryMatchQuery matchQuery) =
    object ["match" .= matchQuery]
  toJSON (QueryMultiMatchQuery multiMatchQuery) =
    toJSON multiMatchQuery
  toJSON (QueryBoolQuery boolQuery) =
    object ["bool" .= boolQuery]
  toJSON (QueryBoostingQuery boostingQuery) =
    object ["boosting" .= boostingQuery]
  toJSON (QueryCommonTermsQuery commonTermsQuery) =
    object ["common" .= commonTermsQuery]
  toJSON (ConstantScoreQuery query boost) =
    object
      [ "constant_score"
          .= object
            [ "filter" .= query,
              "boost" .= boost
            ]
      ]
  toJSON (QueryFunctionScoreQuery functionScoreQuery) =
    object ["function_score" .= functionScoreQuery]
  toJSON (QueryDisMaxQuery disMaxQuery) =
    object ["dis_max" .= disMaxQuery]
  toJSON (QueryFuzzyLikeThisQuery fuzzyQuery) =
    object ["fuzzy_like_this" .= fuzzyQuery]
  toJSON (QueryFuzzyLikeFieldQuery fuzzyFieldQuery) =
    object ["fuzzy_like_this_field" .= fuzzyFieldQuery]
  toJSON (QueryFuzzyQuery fuzzyQuery) =
    object ["fuzzy" .= fuzzyQuery]
  toJSON (QueryHasChildQuery childQuery) =
    object ["has_child" .= childQuery]
  toJSON (QueryHasParentQuery parentQuery) =
    object ["has_parent" .= parentQuery]
  toJSON (QueryIndicesQuery qIndicesQuery) =
    object ["indices" .= qIndicesQuery]
  toJSON (MatchAllQuery boost) =
    object ["match_all" .= omitNulls ["boost" .= boost]]
  toJSON (QueryMoreLikeThisQuery query) =
    object ["more_like_this" .= query]
  toJSON (QueryMoreLikeThisFieldQuery query) =
    object ["more_like_this_field" .= query]
  toJSON (QueryNestedQuery query) =
    object ["nested" .= query]
  toJSON (QueryPrefixQuery query) =
    object ["prefix" .= query]
  toJSON (QueryRangeQuery query) =
    object ["range" .= query]
  toJSON (QueryRegexpQuery query) =
    object ["regexp" .= query]
  toJSON (QuerySimpleQueryStringQuery query) =
    object ["simple_query_string" .= query]
  toJSON (QueryExistsQuery (FieldName fieldName)) =
    object
      [ "exists"
          .= object
            ["field" .= fieldName]
      ]
  toJSON QueryMatchNoneQuery =
    object ["match_none" .= object []]
  toJSON (QueryWildcardQuery query) =
    object ["wildcard" .= query]
  toJSON (QueryKnnQuery query) =
    object ["knn" .= query]

instance FromJSON Query where
  parseJSON v = withObject "Query" parse v
    where
      parse o =
        termQuery
          `taggedWith` "term"
          <|> termsQuery
          `taggedWith` "terms"
          <|> idsQuery
          `taggedWith` "ids"
          <|> queryQueryStringQuery
          `taggedWith` "query_string"
          <|> queryMatchQuery
          `taggedWith` "match"
          <|> queryMultiMatchQuery
          <|> queryBoolQuery
          `taggedWith` "bool"
          <|> queryBoostingQuery
          `taggedWith` "boosting"
          <|> queryCommonTermsQuery
          `taggedWith` "common"
          <|> constantScoreQuery
          `taggedWith` "constant_score"
          <|> queryFunctionScoreQuery
          `taggedWith` "function_score"
          <|> queryDisMaxQuery
          `taggedWith` "dis_max"
          <|> queryFuzzyLikeThisQuery
          `taggedWith` "fuzzy_like_this"
          <|> queryFuzzyLikeFieldQuery
          `taggedWith` "fuzzy_like_this_field"
          <|> queryFuzzyQuery
          `taggedWith` "fuzzy"
          <|> queryHasChildQuery
          `taggedWith` "has_child"
          <|> queryHasParentQuery
          `taggedWith` "has_parent"
          <|> queryIndicesQuery
          `taggedWith` "indices"
          <|> matchAllQuery
          `taggedWith` "match_all"
          <|> queryMoreLikeThisQuery
          `taggedWith` "more_like_this"
          <|> queryMoreLikeThisFieldQuery
          `taggedWith` "more_like_this_field"
          <|> queryNestedQuery
          `taggedWith` "nested"
          <|> queryPrefixQuery
          `taggedWith` "prefix"
          <|> queryRangeQuery
          `taggedWith` "range"
          <|> queryRegexpQuery
          `taggedWith` "regexp"
          <|> querySimpleQueryStringQuery
          `taggedWith` "simple_query_string"
          <|> queryWildcardQuery
          `taggedWith` "wildcard"
        where
          taggedWith parser k = parser =<< o .: k
      termQuery = fieldTagged $ \(FieldName fn) o ->
        TermQuery <$> (Term (fromText fn) <$> o .: "value") <*> o .:? "boost"
      termsQuery o = case HM.toList o of
        [(fn, vs)] -> do
          vals <- parseJSON vs
          case vals of
            x : xs -> return (TermsQuery fn (x :| xs))
            _      -> fail "Expected non empty list of values"
        _ -> fail "Expected object with 1 field-named key"
      idsQuery o = IdsQuery <$> o .: "values"
      queryQueryStringQuery = pure . QueryQueryStringQuery
      queryMatchQuery = pure . QueryMatchQuery
      queryMultiMatchQuery = QueryMultiMatchQuery <$> parseJSON v
      queryBoolQuery = pure . QueryBoolQuery
      queryBoostingQuery = pure . QueryBoostingQuery
      queryCommonTermsQuery = pure . QueryCommonTermsQuery
      constantScoreQuery o = case X.lookup "filter" o of
        Just x ->
          ConstantScoreQuery
            <$> parseJSON x
            <*> o .: "boost"
        _ -> fail "Does not appear to be a ConstantScoreQuery"
      queryFunctionScoreQuery = pure . QueryFunctionScoreQuery
      queryDisMaxQuery = pure . QueryDisMaxQuery
      queryFuzzyLikeThisQuery = pure . QueryFuzzyLikeThisQuery
      queryFuzzyLikeFieldQuery = pure . QueryFuzzyLikeFieldQuery
      queryFuzzyQuery = pure . QueryFuzzyQuery
      queryHasChildQuery = pure . QueryHasChildQuery
      queryHasParentQuery = pure . QueryHasParentQuery
      queryIndicesQuery = pure . QueryIndicesQuery
      matchAllQuery o = MatchAllQuery <$> o .:? "boost"
      queryMoreLikeThisQuery = pure . QueryMoreLikeThisQuery
      queryMoreLikeThisFieldQuery = pure . QueryMoreLikeThisFieldQuery
      queryNestedQuery = pure . QueryNestedQuery
      queryPrefixQuery = pure . QueryPrefixQuery
      queryRangeQuery = pure . QueryRangeQuery
      queryRegexpQuery = pure . QueryRegexpQuery
      querySimpleQueryStringQuery = pure . QuerySimpleQueryStringQuery
      -- queryExistsQuery o = QueryExistsQuery <$> o .: "field"
      queryWildcardQuery = pure . QueryWildcardQuery

-- | As of Elastic 2.0, 'Filters' are just 'Queries' housed in a
--   Bool Query, and flagged in a different context.
newtype Filter = Filter {unFilter :: Query}
  deriving (Eq, Show)

instance ToJSON Filter where
  toJSON = toJSON . unFilter

instance FromJSON Filter where
  parseJSON v = Filter <$> parseJSON v

data RegexpQuery = RegexpQuery
  { regexpQueryField :: FieldName,
    regexpQuery      :: Regexp,
    regexpQueryFlags :: RegexpFlags,
    regexpQueryBoost :: Maybe Boost
  }
  deriving (Eq, Show, Generic)

instance ToJSON RegexpQuery where
  toJSON
    ( RegexpQuery
        (FieldName rqQueryField)
        (Regexp regexpQueryQuery)
        rqQueryFlags
        rqQueryBoost
      ) =
      object [fromText rqQueryField .= omitNulls base]
      where
        base =
          [ "value" .= regexpQueryQuery,
            "flags" .= rqQueryFlags,
            "boost" .= rqQueryBoost
          ]

instance FromJSON RegexpQuery where
  parseJSON = withObject "RegexpQuery" parse
    where
      parse = fieldTagged $ \fn o ->
        RegexpQuery fn
          <$> o .: "value"
          <*> o .: "flags"
          <*> o .:? "boost"

data WildcardQuery = WildcardQuery
  { wildcardQueryField :: FieldName,
    wildcardQuery      :: Key,
    wildcardQueryBoost :: Maybe Boost
  }
  deriving (Eq, Show, Generic)

instance ToJSON WildcardQuery where
  toJSON
    ( WildcardQuery
        (FieldName wcQueryField)
        wcQueryQuery
        wcQueryBoost
      ) =
      object [fromText wcQueryField .= omitNulls base]
      where
        base =
          [ "value" .= wcQueryQuery,
            "boost" .= wcQueryBoost
          ]

instance FromJSON WildcardQuery where
  parseJSON = withObject "WildcardQuery" parse
    where
      parse = fieldTagged $ \fn o ->
        WildcardQuery fn
          <$> o .: "value"
          <*> o .:? "boost"

data KnnQuery = KnnQuery
  { knnQueryK         :: Int
  , knnQueryFieldName :: FieldName
  , knnQueryVector    :: [Double]
  , knnQueryFilter    :: Maybe Filter
  }
  deriving (Eq, Show, Generic)

instance ToJSON KnnQuery where
  toJSON KnnQuery{knnQueryK, knnQueryFieldName, knnQueryVector, knnQueryFilter} =
    object
      [ fromText fn .= omitNulls
        [ "k" .= knnQueryK
        , "filter" .= knnQueryFilter
        , "vector" .= knnQueryVector
        ]
      ]
    where
      FieldName fn = knnQueryFieldName

data RangeQuery = RangeQuery
  { rangeQueryField :: FieldName,
    rangeQueryRange :: RangeValue,
    rangeQueryBoost :: Boost
  }
  deriving (Eq, Show, Generic)

instance ToJSON RangeQuery where
  toJSON (RangeQuery (FieldName fieldName) range boost) =
    object [fromText fieldName .= object conjoined]
    where
      conjoined = ("boost" .= boost) : rangeValueToPair range

instance FromJSON RangeQuery where
  parseJSON = withObject "RangeQuery" parse
    where
      parse = fieldTagged $ \fn o ->
        RangeQuery fn
          <$> parseJSON (Object o)
          <*> o .: "boost"

mkRangeQuery :: FieldName -> RangeValue -> RangeQuery
mkRangeQuery f r = RangeQuery f r (Boost 1.0)

data SimpleQueryStringQuery = SimpleQueryStringQuery
  { simpleQueryStringQuery             :: QueryString,
    simpleQueryStringField             :: Maybe FieldOrFields,
    simpleQueryStringOperator          :: Maybe BooleanOperator,
    simpleQueryStringAnalyzer          :: Maybe Analyzer,
    simpleQueryStringFlags             :: Maybe (NonEmpty SimpleQueryFlag),
    simpleQueryStringLowercaseExpanded :: Maybe LowercaseExpanded,
    simpleQueryStringLocale            :: Maybe Locale
  }
  deriving (Eq, Show, Generic)

instance ToJSON SimpleQueryStringQuery where
  toJSON SimpleQueryStringQuery {..} =
    omitNulls (base ++ maybeAdd)
    where
      base = ["query" .= simpleQueryStringQuery]
      maybeAdd =
        [ "fields" .= simpleQueryStringField,
          "default_operator" .= simpleQueryStringOperator,
          "analyzer" .= simpleQueryStringAnalyzer,
          "flags" .= simpleQueryStringFlags,
          "lowercase_expanded_terms" .= simpleQueryStringLowercaseExpanded,
          "locale" .= simpleQueryStringLocale
        ]

instance FromJSON SimpleQueryStringQuery where
  parseJSON = withObject "SimpleQueryStringQuery" parse
    where
      parse o =
        SimpleQueryStringQuery
          <$> o .: "query"
          <*> o .:? "fields"
          <*> o .:? "default_operator"
          <*> o .:? "analyzer"
          <*> (parseFlags <$> o .:? "flags")
          <*> o .:? "lowercase_expanded_terms"
          <*> o .:? "locale"
      parseFlags (Just (x : xs)) = Just (x :| xs)
      parseFlags _               = Nothing

data SimpleQueryFlag
  = SimpleQueryAll
  | SimpleQueryNone
  | SimpleQueryAnd
  | SimpleQueryOr
  | SimpleQueryPrefix
  | SimpleQueryPhrase
  | SimpleQueryPrecedence
  | SimpleQueryEscape
  | SimpleQueryWhitespace
  | SimpleQueryFuzzy
  | SimpleQueryNear
  | SimpleQuerySlop
  deriving (Eq, Show, Generic)

instance ToJSON SimpleQueryFlag where
  toJSON SimpleQueryAll        = "ALL"
  toJSON SimpleQueryNone       = "NONE"
  toJSON SimpleQueryAnd        = "AND"
  toJSON SimpleQueryOr         = "OR"
  toJSON SimpleQueryPrefix     = "PREFIX"
  toJSON SimpleQueryPhrase     = "PHRASE"
  toJSON SimpleQueryPrecedence = "PRECEDENCE"
  toJSON SimpleQueryEscape     = "ESCAPE"
  toJSON SimpleQueryWhitespace = "WHITESPACE"
  toJSON SimpleQueryFuzzy      = "FUZZY"
  toJSON SimpleQueryNear       = "NEAR"
  toJSON SimpleQuerySlop       = "SLOP"

instance FromJSON SimpleQueryFlag where
  parseJSON = withText "SimpleQueryFlag" parse
    where
      parse "ALL"        = pure SimpleQueryAll
      parse "NONE"       = pure SimpleQueryNone
      parse "AND"        = pure SimpleQueryAnd
      parse "OR"         = pure SimpleQueryOr
      parse "PREFIX"     = pure SimpleQueryPrefix
      parse "PHRASE"     = pure SimpleQueryPhrase
      parse "PRECEDENCE" = pure SimpleQueryPrecedence
      parse "ESCAPE"     = pure SimpleQueryEscape
      parse "WHITESPACE" = pure SimpleQueryWhitespace
      parse "FUZZY"      = pure SimpleQueryFuzzy
      parse "NEAR"       = pure SimpleQueryNear
      parse "SLOP"       = pure SimpleQuerySlop
      parse f            = fail ("Unexpected SimpleQueryFlag: " <> show f)

-- use_dis_max and tie_breaker when fields are plural?
data QueryStringQuery = QueryStringQuery
  { queryStringQuery                    :: QueryString,
    queryStringDefaultField             :: Maybe FieldName,
    queryStringOperator                 :: Maybe BooleanOperator,
    queryStringAnalyzer                 :: Maybe Analyzer,
    queryStringAllowLeadingWildcard     :: Maybe AllowLeadingWildcard,
    queryStringLowercaseExpanded        :: Maybe LowercaseExpanded,
    queryStringEnablePositionIncrements :: Maybe EnablePositionIncrements,
    queryStringFuzzyMaxExpansions       :: Maybe MaxExpansions,
    queryStringFuzziness                :: Maybe Fuzziness,
    queryStringFuzzyPrefixLength        :: Maybe PrefixLength,
    queryStringPhraseSlop               :: Maybe PhraseSlop,
    queryStringBoost                    :: Maybe Boost,
    queryStringAnalyzeWildcard          :: Maybe AnalyzeWildcard,
    queryStringGeneratePhraseQueries    :: Maybe GeneratePhraseQueries,
    queryStringMinimumShouldMatch       :: Maybe MinimumMatch,
    queryStringLenient                  :: Maybe Lenient,
    queryStringLocale                   :: Maybe Locale
  }
  deriving (Eq, Show, Generic)

instance ToJSON QueryStringQuery where
  toJSON
    ( QueryStringQuery
        qsQueryString
        qsDefaultField
        qsOperator
        qsAnalyzer
        qsAllowWildcard
        qsLowercaseExpanded
        qsEnablePositionIncrements
        qsFuzzyMaxExpansions
        qsFuzziness
        qsFuzzyPrefixLength
        qsPhraseSlop
        qsBoost
        qsAnalyzeWildcard
        qsGeneratePhraseQueries
        qsMinimumShouldMatch
        qsLenient
        qsLocale
      ) =
      omitNulls base
      where
        base =
          [ "query" .= qsQueryString,
            "default_field" .= qsDefaultField,
            "default_operator" .= qsOperator,
            "analyzer" .= qsAnalyzer,
            "allow_leading_wildcard" .= qsAllowWildcard,
            "lowercase_expanded_terms" .= qsLowercaseExpanded,
            "enable_position_increments" .= qsEnablePositionIncrements,
            "fuzzy_max_expansions" .= qsFuzzyMaxExpansions,
            "fuzziness" .= qsFuzziness,
            "fuzzy_prefix_length" .= qsFuzzyPrefixLength,
            "phrase_slop" .= qsPhraseSlop,
            "boost" .= qsBoost,
            "analyze_wildcard" .= qsAnalyzeWildcard,
            "auto_generate_phrase_queries" .= qsGeneratePhraseQueries,
            "minimum_should_match" .= qsMinimumShouldMatch,
            "lenient" .= qsLenient,
            "locale" .= qsLocale
          ]

instance FromJSON QueryStringQuery where
  parseJSON = withObject "QueryStringQuery" parse
    where
      parse o =
        QueryStringQuery
          <$> o .: "query"
          <*> o .:? "default_field"
          <*> o .:? "default_operator"
          <*> o .:? "analyzer"
          <*> o .:? "allow_leading_wildcard"
          <*> o .:? "lowercase_expanded_terms"
          <*> o .:? "enable_position_increments"
          <*> o .:? "fuzzy_max_expansions"
          <*> o .:? "fuzziness"
          <*> o .:? "fuzzy_prefix_length"
          <*> o .:? "phrase_slop"
          <*> o .:? "boost"
          <*> o .:? "analyze_wildcard"
          <*> o .:? "auto_generate_phrase_queries"
          <*> o .:? "minimum_should_match"
          <*> o .:? "lenient"
          <*> o .:? "locale"

mkQueryStringQuery :: QueryString -> QueryStringQuery
mkQueryStringQuery qs =
  QueryStringQuery
    qs
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

data FieldOrFields
  = FofField FieldName
  | FofFields (NonEmpty FieldName)
  deriving (Eq, Show, Generic)

instance ToJSON FieldOrFields where
  toJSON (FofField fieldName) =
    toJSON fieldName
  toJSON (FofFields fieldNames) =
    toJSON fieldNames

instance FromJSON FieldOrFields where
  parseJSON v =
    FofField <$> parseJSON v
      <|> FofFields <$> (parseNEJSON =<< parseJSON v)

data PrefixQuery = PrefixQuery
  { prefixQueryField       :: FieldName,
    prefixQueryPrefixValue :: Text,
    prefixQueryBoost       :: Maybe Boost
  }
  deriving (Eq, Show, Generic)

instance ToJSON PrefixQuery where
  toJSON (PrefixQuery (FieldName fieldName) queryValue boost) =
    object [fromText fieldName .= omitNulls base]
    where
      base =
        [ "value" .= queryValue,
          "boost" .= boost
        ]

instance FromJSON PrefixQuery where
  parseJSON = withObject "PrefixQuery" parse
    where
      parse = fieldTagged $ \fn o ->
        PrefixQuery fn
          <$> o .: "value"
          <*> o .:? "boost"

data NestedQuery = NestedQuery
  { nestedQueryPath      :: QueryPath,
    nestedQueryScoreType :: ScoreType,
    nestedQuery          :: Query,
    nestedQueryInnerHits :: Maybe InnerHits
  }
  deriving (Eq, Show, Generic)

instance ToJSON NestedQuery where
  toJSON (NestedQuery nqPath nqScoreType nqQuery nqInnerHits) =
    omitNulls
      [ "path" .= nqPath,
        "score_mode" .= nqScoreType,
        "query" .= nqQuery,
        "inner_hits" .= nqInnerHits
      ]

instance FromJSON NestedQuery where
  parseJSON = withObject "NestedQuery" parse
    where
      parse o =
        NestedQuery
          <$> o .: "path"
          <*> o .: "score_mode"
          <*> o .: "query"
          <*> o .:? "inner_hits"

data MoreLikeThisFieldQuery = MoreLikeThisFieldQuery
  { moreLikeThisFieldText            :: Text,
    moreLikeThisFieldFields          :: FieldName,
    -- default 0.3 (30%)
    moreLikeThisFieldPercentMatch    :: Maybe PercentMatch,
    moreLikeThisFieldMinimumTermFreq :: Maybe MinimumTermFrequency,
    moreLikeThisFieldMaxQueryTerms   :: Maybe MaxQueryTerms,
    moreLikeThisFieldStopWords       :: Maybe (NonEmpty StopWord),
    moreLikeThisFieldMinDocFrequency :: Maybe MinDocFrequency,
    moreLikeThisFieldMaxDocFrequency :: Maybe MaxDocFrequency,
    moreLikeThisFieldMinWordLength   :: Maybe MinWordLength,
    moreLikeThisFieldMaxWordLength   :: Maybe MaxWordLength,
    moreLikeThisFieldBoostTerms      :: Maybe BoostTerms,
    moreLikeThisFieldBoost           :: Maybe Boost,
    moreLikeThisFieldAnalyzer        :: Maybe Analyzer
  }
  deriving (Eq, Show, Generic)

instance ToJSON MoreLikeThisFieldQuery where
  toJSON
    ( MoreLikeThisFieldQuery
        text
        (FieldName fieldName)
        percent
        mtf
        mqt
        stopwords
        mindf
        maxdf
        minwl
        maxwl
        boostTerms
        boost
        analyzer
      ) =
      object [fromText fieldName .= omitNulls base]
      where
        base =
          [ "like_text" .= text,
            "percent_terms_to_match" .= percent,
            "min_term_freq" .= mtf,
            "max_query_terms" .= mqt,
            "stop_words" .= stopwords,
            "min_doc_freq" .= mindf,
            "max_doc_freq" .= maxdf,
            "min_word_length" .= minwl,
            "max_word_length" .= maxwl,
            "boost_terms" .= boostTerms,
            "boost" .= boost,
            "analyzer" .= analyzer
          ]

instance FromJSON MoreLikeThisFieldQuery where
  parseJSON = withObject "MoreLikeThisFieldQuery" parse
    where
      parse = fieldTagged $ \fn o ->
        MoreLikeThisFieldQuery
          <$> o .: "like_text"
          <*> pure fn
          <*> o .:? "percent_terms_to_match"
          <*> o .:? "min_term_freq"
          <*> o .:? "max_query_terms"
          -- <*> (optionalNE =<< o .:? "stop_words")
          <*> o .:? "stop_words"
          <*> o .:? "min_doc_freq"
          <*> o .:? "max_doc_freq"
          <*> o .:? "min_word_length"
          <*> o .:? "max_word_length"
          <*> o .:? "boost_terms"
          <*> o .:? "boost"
          <*> o .:? "analyzer"

-- optionalNE = maybe (pure Nothing) (fmap Just . parseNEJSON)

data MoreLikeThisQuery = MoreLikeThisQuery
  { moreLikeThisText            :: Text,
    moreLikeThisFields          :: Maybe (NonEmpty FieldName),
    -- default 0.3 (30%)
    moreLikeThisPercentMatch    :: Maybe PercentMatch,
    moreLikeThisMinimumTermFreq :: Maybe MinimumTermFrequency,
    moreLikeThisMaxQueryTerms   :: Maybe MaxQueryTerms,
    moreLikeThisStopWords       :: Maybe (NonEmpty StopWord),
    moreLikeThisMinDocFrequency :: Maybe MinDocFrequency,
    moreLikeThisMaxDocFrequency :: Maybe MaxDocFrequency,
    moreLikeThisMinWordLength   :: Maybe MinWordLength,
    moreLikeThisMaxWordLength   :: Maybe MaxWordLength,
    moreLikeThisBoostTerms      :: Maybe BoostTerms,
    moreLikeThisBoost           :: Maybe Boost,
    moreLikeThisAnalyzer        :: Maybe Analyzer
  }
  deriving (Eq, Show, Generic)

instance ToJSON MoreLikeThisQuery where
  toJSON
    ( MoreLikeThisQuery
        text
        fields
        percent
        mtf
        mqt
        stopwords
        mindf
        maxdf
        minwl
        maxwl
        boostTerms
        boost
        analyzer
      ) =
      omitNulls base
      where
        base =
          [ "like_text" .= text,
            "fields" .= fields,
            "percent_terms_to_match" .= percent,
            "min_term_freq" .= mtf,
            "max_query_terms" .= mqt,
            "stop_words" .= stopwords,
            "min_doc_freq" .= mindf,
            "max_doc_freq" .= maxdf,
            "min_word_length" .= minwl,
            "max_word_length" .= maxwl,
            "boost_terms" .= boostTerms,
            "boost" .= boost,
            "analyzer" .= analyzer
          ]

instance FromJSON MoreLikeThisQuery where
  parseJSON = withObject "MoreLikeThisQuery" parse
    where
      parse o =
        MoreLikeThisQuery
          <$> o .: "like_text"
          -- <*> (optionalNE =<< o .:? "fields")
          <*> o .:? "fields"
          <*> o .:? "percent_terms_to_match"
          <*> o .:? "min_term_freq"
          <*> o .:? "max_query_terms"
          -- <*> (optionalNE =<< o .:? "stop_words")
          <*> o .:? "stop_words"
          <*> o .:? "min_doc_freq"
          <*> o .:? "max_doc_freq"
          <*> o .:? "min_word_length"
          <*> o .:? "max_word_length"
          <*> o .:? "boost_terms"
          <*> o .:? "boost"
          <*> o .:? "analyzer"

-- optionalNE = maybe (pure Nothing) (fmap Just . parseNEJSON)

data IndicesQuery = IndicesQuery
  { indicesQueryIndices :: [IndexName],
    indicesQuery        :: Query,
    -- default "all"
    indicesQueryNoMatch :: Maybe Query
  }
  deriving (Eq, Show, Generic)

instance ToJSON IndicesQuery where
  toJSON (IndicesQuery indices query noMatch) =
    omitNulls
      [ "indices" .= indices,
        "no_match_query" .= noMatch,
        "query" .= query
      ]

instance FromJSON IndicesQuery where
  parseJSON = withObject "IndicesQuery" parse
    where
      parse o =
        IndicesQuery
          <$> o .:? "indices" .!= []
          <*> o .: "query"
          <*> o .:? "no_match_query"

data HasParentQuery = HasParentQuery
  { hasParentQueryType      :: RelationName,
    hasParentQuery          :: Query,
    hasParentQueryScore     :: Maybe AggregateParentScore,
    hasParentIgnoreUnmapped :: Maybe IgnoreUnmapped
  }
  deriving (Eq, Show, Generic)

instance ToJSON HasParentQuery where
  toJSON (HasParentQuery queryType query scoreType ignoreUnmapped) =
    omitNulls
      [ "parent_type" .= queryType,
        "score" .= scoreType,
        "query" .= query,
        "ignore_unmapped" .= ignoreUnmapped
      ]

instance FromJSON HasParentQuery where
  parseJSON = withObject "HasParentQuery" parse
    where
      parse o =
        HasParentQuery
          <$> o .: "parent_type"
          <*> o .: "query"
          <*> o .:? "score"
          <*> o .:? "ignore_unmapped"

data HasChildQuery = HasChildQuery
  { hasChildQueryType       :: RelationName,
    hasChildQuery           :: Query,
    hasChildQueryScoreType  :: Maybe ScoreType,
    hasChildIgnoreUnmappped :: Maybe IgnoreUnmapped,
    hasChildMinChildren     :: Maybe MinChildren,
    hasChildMaxChildren     :: Maybe MaxChildren
  }
  deriving (Eq, Show, Generic)

instance ToJSON HasChildQuery where
  toJSON (HasChildQuery queryType query scoreType ignoreUnmapped minChildren maxChildren) =
    omitNulls
      [ "query" .= query,
        "score_mode" .= scoreType,
        "type" .= queryType,
        "min_children" .= minChildren,
        "max_children" .= maxChildren,
        "ignore_unmapped" .= ignoreUnmapped
      ]

instance FromJSON HasChildQuery where
  parseJSON = withObject "HasChildQuery" parse
    where
      parse o =
        HasChildQuery
          <$> o .: "type"
          <*> o .: "query"
          <*> o .:? "score_mode"
          <*> o .:? "ignore_unmapped"
          <*> o .:? "min_children"
          <*> o .:? "max_children"

data ScoreType
  = ScoreTypeMax
  | ScoreTypeSum
  | ScoreTypeAvg
  | ScoreTypeNone
  deriving (Eq, Show, Generic)

instance ToJSON ScoreType where
  toJSON ScoreTypeMax  = "max"
  toJSON ScoreTypeAvg  = "avg"
  toJSON ScoreTypeSum  = "sum"
  toJSON ScoreTypeNone = "none"

instance FromJSON ScoreType where
  parseJSON = withText "ScoreType" parse
    where
      parse "max"  = pure ScoreTypeMax
      parse "avg"  = pure ScoreTypeAvg
      parse "sum"  = pure ScoreTypeSum
      parse "none" = pure ScoreTypeNone
      parse t      = fail ("Unexpected ScoreType: " <> show t)

data FuzzyQuery = FuzzyQuery
  { fuzzyQueryField         :: FieldName,
    fuzzyQueryValue         :: Text,
    fuzzyQueryPrefixLength  :: PrefixLength,
    fuzzyQueryMaxExpansions :: MaxExpansions,
    fuzzyQueryFuzziness     :: Fuzziness,
    fuzzyQueryBoost         :: Maybe Boost
  }
  deriving (Eq, Show, Generic)

instance ToJSON FuzzyQuery where
  toJSON
    ( FuzzyQuery
        (FieldName fieldName)
        queryText
        prefixLength
        maxEx
        fuzziness
        boost
      ) =
      object [fromText fieldName .= omitNulls base]
      where
        base =
          [ "value" .= queryText,
            "fuzziness" .= fuzziness,
            "prefix_length" .= prefixLength,
            "boost" .= boost,
            "max_expansions" .= maxEx
          ]

instance FromJSON FuzzyQuery where
  parseJSON = withObject "FuzzyQuery" parse
    where
      parse = fieldTagged $ \fn o ->
        FuzzyQuery fn
          <$> o .: "value"
          <*> o .: "prefix_length"
          <*> o .: "max_expansions"
          <*> o .: "fuzziness"
          <*> o .:? "boost"

data FuzzyLikeFieldQuery = FuzzyLikeFieldQuery
  { fuzzyLikeField                    :: FieldName,
    -- anaphora is good for the soul.
    fuzzyLikeFieldText                :: Text,
    fuzzyLikeFieldMaxQueryTerms       :: MaxQueryTerms,
    fuzzyLikeFieldIgnoreTermFrequency :: IgnoreTermFrequency,
    fuzzyLikeFieldFuzziness           :: Fuzziness,
    fuzzyLikeFieldPrefixLength        :: PrefixLength,
    fuzzyLikeFieldBoost               :: Boost,
    fuzzyLikeFieldAnalyzer            :: Maybe Analyzer
  }
  deriving (Eq, Show, Generic)

instance ToJSON FuzzyLikeFieldQuery where
  toJSON
    ( FuzzyLikeFieldQuery
        (FieldName fieldName)
        fieldText
        maxTerms
        ignoreFreq
        fuzziness
        prefixLength
        boost
        analyzer
      ) =
      object
        [ fromText fieldName
            .= omitNulls
              [ "like_text" .= fieldText,
                "max_query_terms" .= maxTerms,
                "ignore_tf" .= ignoreFreq,
                "fuzziness" .= fuzziness,
                "prefix_length" .= prefixLength,
                "analyzer" .= analyzer,
                "boost" .= boost
              ]
        ]

instance FromJSON FuzzyLikeFieldQuery where
  parseJSON = withObject "FuzzyLikeFieldQuery" parse
    where
      parse = fieldTagged $ \fn o ->
        FuzzyLikeFieldQuery fn
          <$> o .: "like_text"
          <*> o .: "max_query_terms"
          <*> o .: "ignore_tf"
          <*> o .: "fuzziness"
          <*> o .: "prefix_length"
          <*> o .: "boost"
          <*> o .:? "analyzer"

data FuzzyLikeThisQuery = FuzzyLikeThisQuery
  { fuzzyLikeFields              :: [FieldName],
    fuzzyLikeText                :: Text,
    fuzzyLikeMaxQueryTerms       :: MaxQueryTerms,
    fuzzyLikeIgnoreTermFrequency :: IgnoreTermFrequency,
    fuzzyLikeFuzziness           :: Fuzziness,
    fuzzyLikePrefixLength        :: PrefixLength,
    fuzzyLikeBoost               :: Boost,
    fuzzyLikeAnalyzer            :: Maybe Analyzer
  }
  deriving (Eq, Show, Generic)

instance ToJSON FuzzyLikeThisQuery where
  toJSON
    ( FuzzyLikeThisQuery
        fields
        text
        maxTerms
        ignoreFreq
        fuzziness
        prefixLength
        boost
        analyzer
      ) =
      omitNulls base
      where
        base =
          [ "fields" .= fields,
            "like_text" .= text,
            "max_query_terms" .= maxTerms,
            "ignore_tf" .= ignoreFreq,
            "fuzziness" .= fuzziness,
            "prefix_length" .= prefixLength,
            "analyzer" .= analyzer,
            "boost" .= boost
          ]

instance FromJSON FuzzyLikeThisQuery where
  parseJSON = withObject "FuzzyLikeThisQuery" parse
    where
      parse o =
        FuzzyLikeThisQuery
          <$> o .:? "fields" .!= []
          <*> o .: "like_text"
          <*> o .: "max_query_terms"
          <*> o .: "ignore_tf"
          <*> o .: "fuzziness"
          <*> o .: "prefix_length"
          <*> o .: "boost"
          <*> o .:? "analyzer"

data DisMaxQuery = DisMaxQuery
  { disMaxQueries    :: [Query],
    -- default 0.0
    disMaxTiebreaker :: Tiebreaker,
    disMaxBoost      :: Maybe Boost
  }
  deriving (Eq, Show, Generic)

instance ToJSON DisMaxQuery where
  toJSON (DisMaxQuery queries tiebreaker boost) =
    omitNulls base
    where
      base =
        [ "queries" .= queries,
          "boost" .= boost,
          "tie_breaker" .= tiebreaker
        ]

instance FromJSON DisMaxQuery where
  parseJSON = withObject "DisMaxQuery" parse
    where
      parse o =
        DisMaxQuery
          <$> o .:? "queries" .!= []
          <*> o .: "tie_breaker"
          <*> o .:? "boost"

data MatchQuery = MatchQuery
  { matchQueryField              :: FieldName,
    matchQueryQueryString        :: QueryString,
    matchQueryOperator           :: BooleanOperator,
    matchQueryZeroTerms          :: ZeroTermsQuery,
    matchQueryCutoffFrequency    :: Maybe CutoffFrequency,
    matchQueryMatchType          :: Maybe MatchQueryType,
    matchQueryAnalyzer           :: Maybe Analyzer,
    matchQueryMaxExpansions      :: Maybe MaxExpansions,
    matchQueryLenient            :: Maybe Lenient,
    matchQueryBoost              :: Maybe Boost,
    matchQueryMinimumShouldMatch :: Maybe Text,
    matchQueryFuzziness          :: Maybe Fuzziness
  }
  deriving (Eq, Show, Generic)

instance ToJSON MatchQuery where
  toJSON
    ( MatchQuery
        (FieldName fieldName)
        (QueryString mqQueryString)
        booleanOperator
        zeroTermsQuery
        cutoffFrequency
        matchQueryType
        analyzer
        maxExpansions
        lenient
        boost
        minShouldMatch
        mqFuzziness
      ) =
      object [fromText fieldName .= omitNulls base]
      where
        base =
          [ "query" .= mqQueryString,
            "operator" .= booleanOperator,
            "zero_terms_query" .= zeroTermsQuery,
            "cutoff_frequency" .= cutoffFrequency,
            "type" .= matchQueryType,
            "analyzer" .= analyzer,
            "max_expansions" .= maxExpansions,
            "lenient" .= lenient,
            "boost" .= boost,
            "minimum_should_match" .= minShouldMatch,
            "fuzziness" .= mqFuzziness
          ]

instance FromJSON MatchQuery where
  parseJSON = withObject "MatchQuery" parse
    where
      parse = fieldTagged $ \fn o ->
        MatchQuery fn
          <$> o .: "query"
          <*> o .: "operator"
          <*> o .: "zero_terms_query"
          <*> o .:? "cutoff_frequency"
          <*> o .:? "type"
          <*> o .:? "analyzer"
          <*> o .:? "max_expansions"
          <*> o .:? "lenient"
          <*> o .:? "boost"
          <*> o .:? "minimum_should_match"
          <*> o .:? "fuzziness"

-- | 'mkMatchQuery' is a convenience function that defaults the less common parameters,
--    enabling you to provide only the 'FieldName' and 'QueryString' to make a 'MatchQuery'
mkMatchQuery :: FieldName -> QueryString -> MatchQuery
mkMatchQuery field query = MatchQuery field query Or ZeroTermsNone Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data MatchQueryType
  = MatchPhrase
  | MatchPhrasePrefix
  deriving (Eq, Show, Generic)

instance ToJSON MatchQueryType where
  toJSON MatchPhrase       = "phrase"
  toJSON MatchPhrasePrefix = "phrase_prefix"

instance FromJSON MatchQueryType where
  parseJSON = withText "MatchQueryType" parse
    where
      parse "phrase"        = pure MatchPhrase
      parse "phrase_prefix" = pure MatchPhrasePrefix
      parse t               = fail ("Unexpected MatchQueryType: " <> show t)


data MatchPhraseQuery = MatchPhraseQuery
  { matchPhraseField  :: FieldName
  , matchPhraseString :: QueryString
  } deriving (Eq, Show)

instance ToJSON MatchPhraseQuery where
  toJSON
    (MatchPhraseQuery
      (FieldName field)
      (QueryString q)
    ) = object [fromText field .= q]

data MultiMatchQuery = MultiMatchQuery
  { multiMatchQueryFields          :: [FieldName],
    multiMatchQueryString          :: QueryString,
    multiMatchQueryOperator        :: BooleanOperator,
    multiMatchQueryZeroTerms       :: ZeroTermsQuery,
    multiMatchQueryTiebreaker      :: Maybe Tiebreaker,
    multiMatchQueryType            :: Maybe MultiMatchQueryType,
    multiMatchQueryCutoffFrequency :: Maybe CutoffFrequency,
    multiMatchQueryAnalyzer        :: Maybe Analyzer,
    multiMatchQueryMaxExpansions   :: Maybe MaxExpansions,
    multiMatchQueryLenient         :: Maybe Lenient,
    multiMatchQueryFuzziness       :: Maybe Fuzziness
  }
  deriving (Eq, Show, Generic)

instance ToJSON MultiMatchQuery where
  toJSON
    ( MultiMatchQuery
        fields
        (QueryString query)
        boolOp
        ztQ
        tb
        mmqt
        cf
        analyzer
        maxEx
        lenient
        fuzziness
      ) =
      object ["multi_match" .= omitNulls base]
      where
        base =
          [ "fields" .= fmap toJSON fields,
            "query" .= query,
            "operator" .= boolOp,
            "zero_terms_query" .= ztQ,
            "tie_breaker" .= tb,
            "type" .= mmqt,
            "cutoff_frequency" .= cf,
            "analyzer" .= analyzer,
            "max_expansions" .= maxEx,
            "lenient" .= lenient,
            "fuzziness" .= fuzziness
          ]

instance FromJSON MultiMatchQuery where
  parseJSON = withObject "MultiMatchQuery" parse
    where
      parse raw = do
        o <- raw .: "multi_match"
        MultiMatchQuery
          <$> o .:? "fields" .!= []
          <*> o .: "query"
          <*> o .: "operator"
          <*> o .: "zero_terms_query"
          <*> o .:? "tie_breaker"
          <*> o .:? "type"
          <*> o .:? "cutoff_frequency"
          <*> o .:? "analyzer"
          <*> o .:? "max_expansions"
          <*> o .:? "lenient"
          <*> o .:? "fuzziness"

-- | 'mkMultiMatchQuery' is a convenience function that defaults the less common parameters,
--    enabling you to provide only the list of 'FieldName's and 'QueryString' to
--    make a 'MultiMatchQuery'.
mkMultiMatchQuery :: [FieldName] -> QueryString -> MultiMatchQuery
mkMultiMatchQuery matchFields query =
  MultiMatchQuery
    matchFields
    query
    Or
    ZeroTermsNone
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

data MultiMatchQueryType
  = MultiMatchBestFields
  | MultiMatchMostFields
  | MultiMatchCrossFields
  | MultiMatchPhrase
  | MultiMatchPhrasePrefix
  deriving (Eq, Show, Generic)

instance ToJSON MultiMatchQueryType where
  toJSON MultiMatchBestFields   = "best_fields"
  toJSON MultiMatchMostFields   = "most_fields"
  toJSON MultiMatchCrossFields  = "cross_fields"
  toJSON MultiMatchPhrase       = "phrase"
  toJSON MultiMatchPhrasePrefix = "phrase_prefix"

instance FromJSON MultiMatchQueryType where
  parseJSON = withText "MultiMatchPhrasePrefix" parse
    where
      parse "best_fields" = pure MultiMatchBestFields
      parse "most_fields" = pure MultiMatchMostFields
      parse "cross_fields" = pure MultiMatchCrossFields
      parse "phrase" = pure MultiMatchPhrase
      parse "phrase_prefix" = pure MultiMatchPhrasePrefix
      parse t = fail ("Unexpected MultiMatchPhrasePrefix: " <> show t)

data BoolQuery = BoolQuery
  { boolQueryMustMatch          :: [Query],
    boolQueryFilter             :: [Filter],
    boolQueryMustNotMatch       :: [Query],
    boolQueryShouldMatch        :: [Query],
    boolQueryMinimumShouldMatch :: Maybe MinimumMatch,
    boolQueryBoost              :: Maybe Boost,
    boolQueryDisableCoord       :: Maybe DisableCoord
  }
  deriving (Eq, Show, Generic)

instance ToJSON BoolQuery where
  toJSON (BoolQuery mustM filterM' notM shouldM bqMin boost disableCoord) =
    omitNulls base
    where
      base =
        [ "must" .= mustM,
          "filter" .= filterM',
          "must_not" .= notM,
          "should" .= shouldM,
          "minimum_should_match" .= bqMin,
          "boost" .= boost,
          "disable_coord" .= disableCoord
        ]

instance FromJSON BoolQuery where
  parseJSON = withObject "BoolQuery" parse
    where
      parse o =
        BoolQuery
          <$> o .:? "must" .!= []
          <*> o .:? "filter" .!= []
          <*> o .:? "must_not" .!= []
          <*> o .:? "should" .!= []
          <*> o .:? "minimum_should_match"
          <*> o .:? "boost"
          <*> o .:? "disable_coord"

mkBoolQuery :: [Query] -> [Filter] -> [Query] -> [Query] -> BoolQuery
mkBoolQuery must filt mustNot should =
  BoolQuery must filt mustNot should Nothing Nothing Nothing

data BoostingQuery = BoostingQuery
  { positiveQuery :: Query,
    negativeQuery :: Query,
    negativeBoost :: Boost
  }
  deriving (Eq, Show, Generic)

instance ToJSON BoostingQuery where
  toJSON (BoostingQuery bqPositiveQuery bqNegativeQuery bqNegativeBoost) =
    object
      [ "positive" .= bqPositiveQuery,
        "negative" .= bqNegativeQuery,
        "negative_boost" .= bqNegativeBoost
      ]

instance FromJSON BoostingQuery where
  parseJSON = withObject "BoostingQuery" parse
    where
      parse o =
        BoostingQuery
          <$> o .: "positive"
          <*> o .: "negative"
          <*> o .: "negative_boost"

data CommonTermsQuery = CommonTermsQuery
  { commonField              :: FieldName,
    commonQuery              :: QueryString,
    commonCutoffFrequency    :: CutoffFrequency,
    commonLowFreqOperator    :: BooleanOperator,
    commonHighFreqOperator   :: BooleanOperator,
    commonMinimumShouldMatch :: Maybe CommonMinimumMatch,
    commonBoost              :: Maybe Boost,
    commonAnalyzer           :: Maybe Analyzer,
    commonDisableCoord       :: Maybe DisableCoord
  }
  deriving (Eq, Show, Generic)

instance ToJSON CommonTermsQuery where
  toJSON
    ( CommonTermsQuery
        (FieldName fieldName)
        (QueryString query)
        cf
        lfo
        hfo
        msm
        boost
        analyzer
        disableCoord
      ) =
      object [fromText fieldName .= omitNulls base]
      where
        base =
          [ "query" .= query,
            "cutoff_frequency" .= cf,
            "low_freq_operator" .= lfo,
            "minimum_should_match" .= msm,
            "boost" .= boost,
            "analyzer" .= analyzer,
            "disable_coord" .= disableCoord,
            "high_freq_operator" .= hfo
          ]

instance FromJSON CommonTermsQuery where
  parseJSON = withObject "CommonTermsQuery" parse
    where
      parse = fieldTagged $ \fn o ->
        CommonTermsQuery fn
          <$> o .: "query"
          <*> o .: "cutoff_frequency"
          <*> o .: "low_freq_operator"
          <*> o .: "high_freq_operator"
          <*> o .:? "minimum_should_match"
          <*> o .:? "boost"
          <*> o .:? "analyzer"
          <*> o .:? "disable_coord"

data CommonMinimumMatch
  = CommonMinimumMatchHighLow MinimumMatchHighLow
  | CommonMinimumMatch MinimumMatch
  deriving (Eq, Show, Generic)

instance ToJSON CommonMinimumMatch where
  toJSON (CommonMinimumMatch mm) = toJSON mm
  toJSON (CommonMinimumMatchHighLow (MinimumMatchHighLow lowF highF)) =
    object
      [ "low_freq" .= lowF,
        "high_freq" .= highF
      ]

instance FromJSON CommonMinimumMatch where
  parseJSON v =
    parseMinimum v
      <|> parseMinimumHighLow v
    where
      parseMinimum = fmap CommonMinimumMatch . parseJSON
      parseMinimumHighLow =
        fmap CommonMinimumMatchHighLow
          . withObject
            "CommonMinimumMatchHighLow"
            ( \o ->
                MinimumMatchHighLow
                  <$> o .: "low_freq"
                  <*> o .: "high_freq"
            )

data MinimumMatchHighLow = MinimumMatchHighLow
  { lowFreq  :: MinimumMatch,
    highFreq :: MinimumMatch
  }
  deriving (Eq, Show, Generic)

data ZeroTermsQuery
  = ZeroTermsNone
  | ZeroTermsAll
  deriving (Eq, Show, Generic)

instance ToJSON ZeroTermsQuery where
  toJSON ZeroTermsNone = String "none"
  toJSON ZeroTermsAll  = String "all"

instance FromJSON ZeroTermsQuery where
  parseJSON = withText "ZeroTermsQuery" parse
    where
      parse "none" = pure ZeroTermsNone
      parse "all"  = pure ZeroTermsAll
      parse q      = fail ("Unexpected ZeroTermsQuery: " <> show q)

data RangeExecution
  = RangeExecutionIndex
  | RangeExecutionFielddata
  deriving (Eq, Show, Generic)

-- index for smaller ranges, fielddata for longer ranges
instance ToJSON RangeExecution where
  toJSON RangeExecutionIndex     = "index"
  toJSON RangeExecutionFielddata = "fielddata"

instance FromJSON RangeExecution where
  parseJSON = withText "RangeExecution" parse
    where
      parse "index"     = pure RangeExecutionIndex
      parse "fielddata" = pure RangeExecutionFielddata
      parse t           = error ("Unrecognized RangeExecution " <> show t)

newtype Regexp = Regexp Text deriving (Eq, Show, Generic, FromJSON)

data RegexpFlags
  = AllRegexpFlags
  | NoRegexpFlags
  | SomeRegexpFlags (NonEmpty RegexpFlag)
  deriving (Eq, Show, Generic)

instance ToJSON RegexpFlags where
  toJSON AllRegexpFlags = String "ALL"
  toJSON NoRegexpFlags = String "NONE"
  toJSON (SomeRegexpFlags (h :| fs)) = String $ T.intercalate "|" flagStrs
    where
      flagStrs = map flagStr . nub $ h : fs
      flagStr AnyString    = "ANYSTRING"
      flagStr Automaton    = "AUTOMATON"
      flagStr Complement   = "COMPLEMENT"
      flagStr Empty        = "EMPTY"
      flagStr Intersection = "INTERSECTION"
      flagStr Interval     = "INTERVAL"

instance FromJSON RegexpFlags where
  parseJSON = withText "RegexpFlags" parse
    where
      parse "ALL" = pure AllRegexpFlags
      parse "NONE" = pure NoRegexpFlags
      parse t = SomeRegexpFlags <$> parseNEJSON (String <$> T.splitOn "|" t)

data RegexpFlag
  = AnyString
  | Automaton
  | Complement
  | Empty
  | Intersection
  | Interval
  deriving (Eq, Show, Generic)

instance FromJSON RegexpFlag where
  parseJSON = withText "RegexpFlag" parse
    where
      parse "ANYSTRING"    = pure AnyString
      parse "AUTOMATON"    = pure Automaton
      parse "COMPLEMENT"   = pure Complement
      parse "EMPTY"        = pure Empty
      parse "INTERSECTION" = pure Intersection
      parse "INTERVAL"     = pure Interval
      parse f              = fail ("Unknown RegexpFlag: " <> show f)

newtype LessThan = LessThan Double deriving (Eq, Show, Generic)

newtype LessThanEq = LessThanEq Double deriving (Eq, Show, Generic)

newtype GreaterThan = GreaterThan Double deriving (Eq, Show, Generic)

newtype GreaterThanEq = GreaterThanEq Double deriving (Eq, Show, Generic)

newtype LessThanD = LessThanD UTCTime deriving (Eq, Show, Generic)

newtype LessThanEqD = LessThanEqD UTCTime deriving (Eq, Show, Generic)

newtype GreaterThanD = GreaterThanD UTCTime deriving (Eq, Show, Generic)

newtype GreaterThanEqD = GreaterThanEqD UTCTime deriving (Eq, Show, Generic)

data RangeValue
  = RangeDateLte LessThanEqD
  | RangeDateLt LessThanD
  | RangeDateGte GreaterThanEqD
  | RangeDateGt GreaterThanD
  | RangeDateGtLt GreaterThanD LessThanD
  | RangeDateGteLte GreaterThanEqD LessThanEqD
  | RangeDateGteLt GreaterThanEqD LessThanD
  | RangeDateGtLte GreaterThanD LessThanEqD
  | RangeDoubleLte LessThanEq
  | RangeDoubleLt LessThan
  | RangeDoubleGte GreaterThanEq
  | RangeDoubleGt GreaterThan
  | RangeDoubleGtLt GreaterThan LessThan
  | RangeDoubleGteLte GreaterThanEq LessThanEq
  | RangeDoubleGteLt GreaterThanEq LessThan
  | RangeDoubleGtLte GreaterThan LessThanEq
  deriving (Eq, Show, Generic)

parseRangeValue ::
  ( FromJSON t4,
    FromJSON t3,
    FromJSON t2,
    FromJSON t1
  ) =>
  (t3 -> t5) ->
  (t1 -> t6) ->
  (t4 -> t7) ->
  (t2 -> t8) ->
  (t5 -> t6 -> b) ->
  (t7 -> t6 -> b) ->
  (t5 -> t8 -> b) ->
  (t7 -> t8 -> b) ->
  (t5 -> b) ->
  (t6 -> b) ->
  (t7 -> b) ->
  (t8 -> b) ->
  Parser b ->
  Object ->
  Parser b
parseRangeValue
  mkGt
  mkLt
  mkGte
  mkLte
  fGtLt
  fGteLt
  fGtLte
  fGteLte
  fGt
  fLt
  fGte
  fLte
  nada
  o = do
    lt <- o .:? "lt"
    lte <- o .:? "lte"
    gt <- o .:? "gt"
    gte <- o .:? "gte"
    case (lt, lte, gt, gte) of
      (Just a, _, Just b, _) ->
        return (fGtLt (mkGt b) (mkLt a))
      (Just a, _, _, Just b) ->
        return (fGteLt (mkGte b) (mkLt a))
      (_, Just a, Just b, _) ->
        return (fGtLte (mkGt b) (mkLte a))
      (_, Just a, _, Just b) ->
        return (fGteLte (mkGte b) (mkLte a))
      (_, _, Just a, _) ->
        return (fGt (mkGt a))
      (Just a, _, _, _) ->
        return (fLt (mkLt a))
      (_, _, _, Just a) ->
        return (fGte (mkGte a))
      (_, Just a, _, _) ->
        return (fLte (mkLte a))
      (Nothing, Nothing, Nothing, Nothing) ->
        nada

instance FromJSON RangeValue where
  parseJSON = withObject "RangeValue" parse
    where
      parse o =
        parseDate o
          <|> parseDouble o
      parseDate o =
        parseRangeValue
          GreaterThanD
          LessThanD
          GreaterThanEqD
          LessThanEqD
          RangeDateGtLt
          RangeDateGteLt
          RangeDateGtLte
          RangeDateGteLte
          RangeDateGt
          RangeDateLt
          RangeDateGte
          RangeDateLte
          mzero
          o
      parseDouble o =
        parseRangeValue
          GreaterThan
          LessThan
          GreaterThanEq
          LessThanEq
          RangeDoubleGtLt
          RangeDoubleGteLt
          RangeDoubleGtLte
          RangeDoubleGteLte
          RangeDoubleGt
          RangeDoubleLt
          RangeDoubleGte
          RangeDoubleLte
          mzero
          o

rangeValueToPair :: RangeValue -> [Pair]
rangeValueToPair rv = case rv of
  RangeDateLte (LessThanEqD t)                       -> ["lte" .= t]
  RangeDateGte (GreaterThanEqD t)                    -> ["gte" .= t]
  RangeDateLt (LessThanD t)                          -> ["lt" .= t]
  RangeDateGt (GreaterThanD t)                       -> ["gt" .= t]
  RangeDateGteLte (GreaterThanEqD l) (LessThanEqD g) -> ["gte" .= l, "lte" .= g]
  RangeDateGtLte (GreaterThanD l) (LessThanEqD g)    -> ["gt" .= l, "lte" .= g]
  RangeDateGteLt (GreaterThanEqD l) (LessThanD g)    -> ["gte" .= l, "lt" .= g]
  RangeDateGtLt (GreaterThanD l) (LessThanD g)       -> ["gt" .= l, "lt" .= g]
  RangeDoubleLte (LessThanEq t)                      -> ["lte" .= t]
  RangeDoubleGte (GreaterThanEq t)                   -> ["gte" .= t]
  RangeDoubleLt (LessThan t)                         -> ["lt" .= t]
  RangeDoubleGt (GreaterThan t)                      -> ["gt" .= t]
  RangeDoubleGteLte (GreaterThanEq l) (LessThanEq g) -> ["gte" .= l, "lte" .= g]
  RangeDoubleGtLte (GreaterThan l) (LessThanEq g)    -> ["gt" .= l, "lte" .= g]
  RangeDoubleGteLt (GreaterThanEq l) (LessThan g)    -> ["gte" .= l, "lt" .= g]
  RangeDoubleGtLt (GreaterThan l) (LessThan g)       -> ["gt" .= l, "lt" .= g]

data Term = Term
  { termField :: Key,
    termValue :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON Term where
  toJSON (Term field value) =
    object
      [ "term"
          .= object
            [field .= value]
      ]

instance FromJSON Term where
  parseJSON = withObject "Term" parse
    where
      parse o = do
        termObj <- o .: "term"
        case HM.toList termObj of
          [(fn, v)] -> Term fn <$> parseJSON v
          _         -> fail "Expected object with 1 field-named key"

data BoolMatch
  = MustMatch Term Cache
  | MustNotMatch Term Cache
  | ShouldMatch [Term] Cache
  deriving (Eq, Show, Generic)

instance ToJSON BoolMatch where
  toJSON (MustMatch term cache) =
    object
      [ "must" .= term,
        "_cache" .= cache
      ]
  toJSON (MustNotMatch term cache) =
    object
      [ "must_not" .= term,
        "_cache" .= cache
      ]
  toJSON (ShouldMatch terms cache) =
    object
      [ "should" .= fmap toJSON terms,
        "_cache" .= cache
      ]

instance FromJSON BoolMatch where
  parseJSON = withObject "BoolMatch" parse
    where
      parse o =
        mustMatch
          `taggedWith` "must"
          <|> mustNotMatch
          `taggedWith` "must_not"
          <|> shouldMatch
          `taggedWith` "should"
        where
          taggedWith parser k = parser =<< o .: k
          mustMatch t = MustMatch t <$> o .:? "_cache" .!= defaultCache
          mustNotMatch t = MustNotMatch t <$> o .:? "_cache" .!= defaultCache
          shouldMatch t = ShouldMatch t <$> o .:? "_cache" .!= defaultCache

-- "memory" or "indexed"
data GeoFilterType
  = GeoFilterMemory
  | GeoFilterIndexed
  deriving (Eq, Show, Generic)

instance ToJSON GeoFilterType where
  toJSON GeoFilterMemory  = String "memory"
  toJSON GeoFilterIndexed = String "indexed"

instance FromJSON GeoFilterType where
  parseJSON = withText "GeoFilterType" parse
    where
      parse "memory"  = pure GeoFilterMemory
      parse "indexed" = pure GeoFilterIndexed
      parse t         = fail ("Unrecognized GeoFilterType: " <> show t)

data LatLon = LatLon
  { lat :: Double,
    lon :: Double
  }
  deriving (Eq, Show, Generic)

instance ToJSON LatLon where
  toJSON (LatLon lLat lLon) =
    object
      [ "lat" .= lLat,
        "lon" .= lLon
      ]

instance FromJSON LatLon where
  parseJSON = withObject "LatLon" parse
    where
      parse o =
        LatLon
          <$> o .: "lat"
          <*> o .: "lon"

data GeoBoundingBox = GeoBoundingBox
  { topLeft     :: LatLon,
    bottomRight :: LatLon
  }
  deriving (Eq, Show, Generic)

instance ToJSON GeoBoundingBox where
  toJSON (GeoBoundingBox gbbTopLeft gbbBottomRight) =
    object
      [ "top_left" .= gbbTopLeft,
        "bottom_right" .= gbbBottomRight
      ]

instance FromJSON GeoBoundingBox where
  parseJSON = withObject "GeoBoundingBox" parse
    where
      parse o =
        GeoBoundingBox
          <$> o .: "top_left"
          <*> o .: "bottom_right"

data GeoBoundingBoxConstraint = GeoBoundingBoxConstraint
  { geoBBField        :: FieldName,
    constraintBox     :: GeoBoundingBox,
    bbConstraintcache :: Cache,
    geoType           :: GeoFilterType
  }
  deriving (Eq, Show, Generic)

instance ToJSON GeoBoundingBoxConstraint where
  toJSON
    ( GeoBoundingBoxConstraint
        (FieldName gbbcGeoBBField)
        gbbcConstraintBox
        cache
        type'
      ) =
      object
        [ fromText gbbcGeoBBField .= gbbcConstraintBox,
          "_cache" .= cache,
          "type" .= type'
        ]

instance FromJSON GeoBoundingBoxConstraint where
  parseJSON = withObject "GeoBoundingBoxConstraint" parse
    where
      parse o = case X.toList (deleteSeveral ["type", "_cache"] o) of
        [(fn, v)] ->
          GeoBoundingBoxConstraint (FieldName $ toText fn)
            <$> parseJSON v
            <*> o .:? "_cache" .!= defaultCache
            <*> o .: "type"
        _ -> fail "Could not find field name for GeoBoundingBoxConstraint"

data GeoPoint = GeoPoint
  { geoField :: FieldName,
    latLon   :: LatLon
  }
  deriving (Eq, Show, Generic)

instance ToJSON GeoPoint where
  toJSON (GeoPoint (FieldName geoPointField) geoPointLatLon) =
    object [fromText geoPointField .= geoPointLatLon]

data DistanceUnit
  = Miles
  | Yards
  | Feet
  | Inches
  | Kilometers
  | Meters
  | Centimeters
  | Millimeters
  | NauticalMiles
  deriving (Eq, Show, Generic)

instance ToJSON DistanceUnit where
  toJSON Miles         = String "mi"
  toJSON Yards         = String "yd"
  toJSON Feet          = String "ft"
  toJSON Inches        = String "in"
  toJSON Kilometers    = String "km"
  toJSON Meters        = String "m"
  toJSON Centimeters   = String "cm"
  toJSON Millimeters   = String "mm"
  toJSON NauticalMiles = String "nmi"

instance FromJSON DistanceUnit where
  parseJSON = withText "DistanceUnit" parse
    where
      parse "mi"  = pure Miles
      parse "yd"  = pure Yards
      parse "ft"  = pure Feet
      parse "in"  = pure Inches
      parse "km"  = pure Kilometers
      parse "m"   = pure Meters
      parse "cm"  = pure Centimeters
      parse "mm"  = pure Millimeters
      parse "nmi" = pure NauticalMiles
      parse u     = fail ("Unrecognized DistanceUnit: " <> show u)

data DistanceType
  = Arc
  | SloppyArc -- doesn't exist <1.0
  | Plane
  deriving (Eq, Show, Generic)

instance ToJSON DistanceType where
  toJSON Arc       = String "arc"
  toJSON SloppyArc = String "sloppy_arc"
  toJSON Plane     = String "plane"

instance FromJSON DistanceType where
  parseJSON = withText "DistanceType" parse
    where
      parse "arc"        = pure Arc
      parse "sloppy_arc" = pure SloppyArc
      parse "plane"      = pure Plane
      parse t            = fail ("Unrecognized DistanceType: " <> show t)

data OptimizeBbox
  = OptimizeGeoFilterType GeoFilterType
  | NoOptimizeBbox
  deriving (Eq, Show, Generic)

instance ToJSON OptimizeBbox where
  toJSON NoOptimizeBbox              = String "none"
  toJSON (OptimizeGeoFilterType gft) = toJSON gft

instance FromJSON OptimizeBbox where
  parseJSON v =
    withText "NoOptimizeBbox" parseNoOptimize v
      <|> parseOptimize v
    where
      parseNoOptimize "none" = pure NoOptimizeBbox
      parseNoOptimize _      = mzero
      parseOptimize = fmap OptimizeGeoFilterType . parseJSON

data Distance = Distance
  { coefficient :: Double,
    unit        :: DistanceUnit
  }
  deriving (Eq, Show, Generic)

instance ToJSON Distance where
  toJSON (Distance dCoefficient dUnit) =
    String boltedTogether
    where
      coefText = showText dCoefficient
      unitText = case toJSON dUnit of
        String s -> s
        _        -> error "ToJSON Distance dunit"
      boltedTogether = mappend coefText unitText

instance FromJSON Distance where
  parseJSON = withText "Distance" parse
    where
      parse t =
        Distance
          <$> parseCoeff nT
          <*> parseJSON (String unitT)
        where
          (nT, unitT) = T.span validForNumber t
          -- may be a better way to do this
          validForNumber '-' = True
          validForNumber '.' = True
          validForNumber 'e' = True
          validForNumber c   = isNumber c
          parseCoeff "" = fail "Empty string cannot be parsed as number"
          parseCoeff s  = return (read (T.unpack s))

data DistanceRange = DistanceRange
  { distanceFrom :: Distance,
    distanceTo   :: Distance
  }
  deriving (Eq, Show, Generic)

type TemplateQueryValue = Text

newtype TemplateQueryKeyValuePairs
  = TemplateQueryKeyValuePairs (X.KeyMap TemplateQueryValue)
  deriving (Eq, Show)

instance ToJSON TemplateQueryKeyValuePairs where
  toJSON (TemplateQueryKeyValuePairs x) = Object $ String <$> x

instance FromJSON TemplateQueryKeyValuePairs where
  parseJSON (Object o) =
    pure . TemplateQueryKeyValuePairs $ X.mapMaybe getValue o
    where
      getValue (String x) = Just x
      getValue _          = Nothing
  parseJSON _ =
    fail "error parsing TemplateQueryKeyValuePairs"

-- | 'BooleanOperator' is the usual And/Or operators with an ES compatible
--    JSON encoding baked in. Used all over the place.
data BooleanOperator = And | Or deriving (Eq, Show, Generic)

instance ToJSON BooleanOperator where
  toJSON And = String "and"
  toJSON Or  = String "or"

instance FromJSON BooleanOperator where
  parseJSON = withText "BooleanOperator" parse
    where
      parse "and" = pure And
      parse "or"  = pure Or
      parse o     = fail ("Unexpected BooleanOperator: " <> show o)

-- | 'Cache' is for telling ES whether it should cache a 'Filter' not.
--    'Query's cannot be cached.
type Cache = Bool -- caching on/off

defaultCache :: Cache
defaultCache = False

data FunctionScoreQuery = FunctionScoreQuery
  { functionScoreQuery     :: Maybe Query,
    functionScoreBoost     :: Maybe Boost,
    functionScoreFunctions :: FunctionScoreFunctions,
    functionScoreMaxBoost  :: Maybe Boost,
    functionScoreBoostMode :: Maybe BoostMode,
    functionScoreMinScore  :: Score,
    functionScoreScoreMode :: Maybe ScoreMode
  }
  deriving (Eq, Show, Generic)

instance ToJSON FunctionScoreQuery where
  toJSON (FunctionScoreQuery query boost fns maxBoost boostMode minScore scoreMode) =
    omitNulls base
    where
      base =
        functionScoreFunctionsPair fns
          : [ "query" .= query,
              "boost" .= boost,
              "max_boost" .= maxBoost,
              "boost_mode" .= boostMode,
              "min_score" .= minScore,
              "score_mode" .= scoreMode
            ]

instance FromJSON FunctionScoreQuery where
  parseJSON = withObject "FunctionScoreQuery" parse
    where
      parse o =
        FunctionScoreQuery
          <$> o .:? "query"
          <*> o .:? "boost"
          <*> ( singleFunction o
                  <|> multipleFunctions
                  `taggedWith` "functions"
              )
          <*> o .:? "max_boost"
          <*> o .:? "boost_mode"
          <*> o .:? "min_score"
          <*> o .:? "score_mode"
        where
          taggedWith parser k = parser =<< o .: k
      singleFunction = fmap FunctionScoreSingle . parseFunctionScoreFunction
      multipleFunctions = pure . FunctionScoreMultiple

data FunctionScoreFunctions
  = FunctionScoreSingle FunctionScoreFunction
  | FunctionScoreMultiple (NonEmpty ComponentFunctionScoreFunction)
  deriving (Eq, Show, Generic)

data ComponentFunctionScoreFunction = ComponentFunctionScoreFunction
  { componentScoreFunctionFilter :: Maybe Filter,
    componentScoreFunction       :: FunctionScoreFunction,
    componentScoreFunctionWeight :: Maybe Weight
  }
  deriving (Eq, Show, Generic)

instance ToJSON ComponentFunctionScoreFunction where
  toJSON (ComponentFunctionScoreFunction filter' fn weight) =
    omitNulls base
    where
      base =
        functionScoreFunctionPair fn
          : [ "filter" .= filter',
              "weight" .= weight
            ]

instance FromJSON ComponentFunctionScoreFunction where
  parseJSON = withObject "ComponentFunctionScoreFunction" parse
    where
      parse o =
        ComponentFunctionScoreFunction
          <$> o .:? "filter"
          <*> parseFunctionScoreFunction o
          <*> o .:? "weight"

functionScoreFunctionsPair :: FunctionScoreFunctions -> (Key, Value)
functionScoreFunctionsPair (FunctionScoreSingle fn) =
  functionScoreFunctionPair fn
functionScoreFunctionsPair (FunctionScoreMultiple componentFns) =
  ("functions", toJSON componentFns)

fieldTagged :: (Monad m, MonadFail m) => (FieldName -> Object -> m a) -> Object -> m a
fieldTagged f o = case X.toList o of
  [(k, Object o')] -> f (FieldName $ toText k) o'
  _                -> fail "Expected object with 1 field-named key"

-- | Fuzziness value as a number or 'AUTO'.
-- See:
-- https://www.elastic.co/guide/en/elasticsearch/reference/current/common-options.html#fuzziness
data Fuzziness = Fuzziness Double | FuzzinessAuto
  deriving (Eq, Show, Generic)

instance ToJSON Fuzziness where
  toJSON (Fuzziness n) = toJSON n
  toJSON FuzzinessAuto = String "AUTO"

instance FromJSON Fuzziness where
  parseJSON (String "AUTO") = return FuzzinessAuto
  parseJSON v               = Fuzziness <$> parseJSON v

data InnerHits = InnerHits
  { innerHitsFrom      :: Maybe Integer
  , innerHitsSize      :: Maybe Integer
  , innerHitsName      :: Maybe Text
  , innerHitsHighlight :: Maybe Highlights
  , innerHitsSource    :: Maybe Source
  }
  deriving (Eq, Show, Generic)

instance ToJSON InnerHits where
  toJSON (InnerHits ihFrom ihSize ihName ihHighlights ihSource) =
    omitNulls
      [ "from" .= ihFrom,
        "size" .= ihSize,
        "name" .= ihName,
        "highlight" .= ihHighlights,
        "_source" .= ihSource
      ]

instance FromJSON InnerHits where
  parseJSON = withObject "InnerHits" parse
    where
      parse o =
        InnerHits
          <$> o .:? "from"
          <*> o .:? "size"
          <*> o .:? "name"
          <*> pure Nothing
          <*> pure Nothing

mkInnerHits :: Maybe Highlights -> Maybe Source -> InnerHits
mkInnerHits = InnerHits Nothing Nothing Nothing

-- database.bloodhound.internal.suggest
--

type HitHighlight = M.Map Text [Text]

data Highlights = Highlights
  { globalsettings  :: Maybe HighlightSettings,
    highlightFields :: [FieldHighlight]
  }
  deriving (Eq, Show, Generic)

instance ToJSON Highlights where
  toJSON (Highlights global fields) =
    omitNulls
      ( ("fields" .= fields)
          : highlightSettingsPairs global
      )

data FieldHighlight
  = FieldHighlight FieldName (Maybe HighlightSettings)
  deriving (Eq, Show, Generic)

instance ToJSON FieldHighlight where
  toJSON (FieldHighlight (FieldName fName) (Just fSettings)) =
    object [fromText fName .= fSettings]
  toJSON (FieldHighlight (FieldName fName) Nothing) =
    object [fromText fName .= emptyObject]

data HighlightSettings
  = Plain PlainHighlight
  | Postings PostingsHighlight
  | FastVector FastVectorHighlight
  deriving (Eq, Show, Generic)

instance ToJSON HighlightSettings where
  toJSON hs = omitNulls (highlightSettingsPairs (Just hs))

data PlainHighlight = PlainHighlight
  { plainCommon  :: Maybe CommonHighlight,
    plainNonPost :: Maybe NonPostings
  }
  deriving (Eq, Show, Generic)

-- This requires that index_options are set to 'offset' in the mapping.
newtype PostingsHighlight
  = PostingsHighlight (Maybe CommonHighlight)
  deriving (Eq, Show, Generic)

-- This requires that term_vector is set to 'with_positions_offsets' in the mapping.
data FastVectorHighlight = FastVectorHighlight
  { fvCommon          :: Maybe CommonHighlight,
    fvNonPostSettings :: Maybe NonPostings,
    boundaryChars     :: Maybe Text,
    boundaryMaxScan   :: Maybe Int,
    fragmentOffset    :: Maybe Int,
    matchedFields     :: [Text],
    phraseLimit       :: Maybe Int
  }
  deriving (Eq, Show, Generic)

data CommonHighlight = CommonHighlight
  { order             :: Maybe Text,
    forceSource       :: Maybe Bool,
    tag               :: Maybe HighlightTag,
    encoder           :: Maybe HighlightEncoder,
    noMatchSize       :: Maybe Int,
    highlightQuery    :: Maybe Query,
    requireFieldMatch :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

-- Settings that are only applicable to FastVector and Plain highlighters.
data NonPostings = NonPostings
  { fragmentSize      :: Maybe Int,
    numberOfFragments :: Maybe Int
  }
  deriving (Eq, Show, Generic)

data HighlightEncoder
  = DefaultEncoder
  | HTMLEncoder
  deriving (Eq, Show, Generic)

instance ToJSON HighlightEncoder where
  toJSON DefaultEncoder = String "default"
  toJSON HTMLEncoder    = String "html"

-- NOTE: Should the tags use some kind of HTML type, rather than Text?
data HighlightTag
  = TagSchema Text
  | -- Only uses more than the first value in the lists if fvh
    CustomTags ([Text], [Text])
  deriving (Eq, Show, Generic)

highlightSettingsPairs :: Maybe HighlightSettings -> [Pair]
highlightSettingsPairs Nothing                 = []
highlightSettingsPairs (Just (Plain plh))      = plainHighPairs (Just plh)
highlightSettingsPairs (Just (Postings ph))    = postHighPairs (Just ph)
highlightSettingsPairs (Just (FastVector fvh)) = fastVectorHighPairs (Just fvh)

plainHighPairs :: Maybe PlainHighlight -> [Pair]
plainHighPairs Nothing = []
plainHighPairs (Just (PlainHighlight plCom plNonPost)) =
  ["type" .= String "plain"]
    ++ commonHighlightPairs plCom
    ++ nonPostingsToPairs plNonPost

postHighPairs :: Maybe PostingsHighlight -> [Pair]
postHighPairs Nothing = []
postHighPairs (Just (PostingsHighlight pCom)) =
  ("type" .= String "postings")
    : commonHighlightPairs pCom

fastVectorHighPairs :: Maybe FastVectorHighlight -> [Pair]
fastVectorHighPairs Nothing = []
fastVectorHighPairs
  ( Just
      ( FastVectorHighlight
          fvCom
          fvNonPostSettings'
          fvBoundChars
          fvBoundMaxScan
          fvFragOff
          fvMatchedFields
          fvPhraseLim
        )
    ) =
    [ "type" .= String "fvh",
      "boundary_chars" .= fvBoundChars,
      "boundary_max_scan" .= fvBoundMaxScan,
      "fragment_offset" .= fvFragOff,
      "matched_fields" .= fvMatchedFields,
      "phraseLimit" .= fvPhraseLim
    ]
      ++ commonHighlightPairs fvCom
      ++ nonPostingsToPairs fvNonPostSettings'

commonHighlightPairs :: Maybe CommonHighlight -> [Pair]
commonHighlightPairs Nothing = []
commonHighlightPairs
  ( Just
      ( CommonHighlight
          chScore
          chForceSource
          chTag
          chEncoder
          chNoMatchSize
          chHighlightQuery
          chRequireFieldMatch
        )
    ) =
    [ "order" .= chScore,
      "force_source" .= chForceSource,
      "encoder" .= chEncoder,
      "no_match_size" .= chNoMatchSize,
      "highlight_query" .= chHighlightQuery,
      "require_fieldMatch" .= chRequireFieldMatch
    ]
      ++ highlightTagToPairs chTag

nonPostingsToPairs :: Maybe NonPostings -> [Pair]
nonPostingsToPairs Nothing = []
nonPostingsToPairs (Just (NonPostings npFragSize npNumOfFrags)) =
  [ "fragment_size" .= npFragSize,
    "number_of_fragments" .= npNumOfFrags
  ]

highlightTagToPairs :: Maybe HighlightTag -> [Pair]
highlightTagToPairs (Just (TagSchema _)) =
  [ "scheme" .= String "default"
  ]
highlightTagToPairs (Just (CustomTags (pre, post))) =
  [ "pre_tags" .= pre,
    "post_tags" .= post
  ]
highlightTagToPairs Nothing = []

-- database.bloodhound.internal.suggest



data Suggest = Suggest
  { suggestText :: Text,
    suggestName :: Text,
    suggestType :: SuggestType
  }
  deriving (Eq, Show, Generic)

instance ToJSON Suggest where
  toJSON Suggest {..} =
    object
      [ "text" .= suggestText,
        fromText suggestName .= suggestType
      ]

instance FromJSON Suggest where
  parseJSON (Object o) = do
    suggestText' <- o .: "text"
    let dropTextList =
          X.toList $
            X.filterWithKey (\x _ -> x /= "text") o
    suggestName' <-
      case dropTextList of
        [(x, _)] -> return x
        _        -> fail "error parsing Suggest field name"
    suggestType' <- o .: suggestName'
    return $ Suggest suggestText' (toText suggestName') suggestType'
  parseJSON x = typeMismatch "Suggest" x

newtype SuggestType
  = SuggestTypePhraseSuggester PhraseSuggester
  deriving (Eq, Show, Generic)

instance ToJSON SuggestType where
  toJSON (SuggestTypePhraseSuggester x) =
    object ["phrase" .= x]

instance FromJSON SuggestType where
  parseJSON = withObject "SuggestType" parse
    where
      parse o = phraseSuggester `taggedWith` "phrase"
        where
          taggedWith parser k = parser =<< o .: k
          phraseSuggester = pure . SuggestTypePhraseSuggester

data PhraseSuggester = PhraseSuggester
  { phraseSuggesterField                   :: FieldName,
    phraseSuggesterGramSize                :: Maybe Int,
    phraseSuggesterRealWordErrorLikelihood :: Maybe Int,
    phraseSuggesterConfidence              :: Maybe Int,
    phraseSuggesterMaxErrors               :: Maybe Int,
    phraseSuggesterSeparator               :: Maybe Text,
    phraseSuggesterSize                    :: Maybe Size,
    phraseSuggesterAnalyzer                :: Maybe Analyzer,
    phraseSuggesterShardSize               :: Maybe Int,
    phraseSuggesterHighlight               :: Maybe PhraseSuggesterHighlighter,
    phraseSuggesterCollate                 :: Maybe PhraseSuggesterCollate,
    phraseSuggesterCandidateGenerators     :: [DirectGenerators]
  }
  deriving (Eq, Show, Generic)

instance ToJSON PhraseSuggester where
  toJSON PhraseSuggester {..} =
    omitNulls
      [ "field" .= phraseSuggesterField,
        "gram_size" .= phraseSuggesterGramSize,
        "real_word_error_likelihood"
          .= phraseSuggesterRealWordErrorLikelihood,
        "confidence" .= phraseSuggesterConfidence,
        "max_errors" .= phraseSuggesterMaxErrors,
        "separator" .= phraseSuggesterSeparator,
        "size" .= phraseSuggesterSize,
        "analyzer" .= phraseSuggesterAnalyzer,
        "shard_size" .= phraseSuggesterShardSize,
        "highlight" .= phraseSuggesterHighlight,
        "collate" .= phraseSuggesterCollate,
        "direct_generator"
          .= phraseSuggesterCandidateGenerators
      ]

instance FromJSON PhraseSuggester where
  parseJSON = withObject "PhraseSuggester" parse
    where
      parse o =
        PhraseSuggester
          <$> o .: "field"
          <*> o .:? "gram_size"
          <*> o .:? "real_word_error_likelihood"
          <*> o .:? "confidence"
          <*> o .:? "max_errors"
          <*> o .:? "separator"
          <*> o .:? "size"
          <*> o .:? "analyzer"
          <*> o .:? "shard_size"
          <*> o .:? "highlight"
          <*> o .:? "collate"
          <*> o .:? "direct_generator" .!= []

mkPhraseSuggester :: FieldName -> PhraseSuggester
mkPhraseSuggester fName =
  PhraseSuggester
    fName
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    []

data PhraseSuggesterHighlighter = PhraseSuggesterHighlighter
  { phraseSuggesterHighlighterPreTag  :: Text,
    phraseSuggesterHighlighterPostTag :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON PhraseSuggesterHighlighter where
  toJSON PhraseSuggesterHighlighter {..} =
    object
      [ "pre_tag" .= phraseSuggesterHighlighterPreTag,
        "post_tag" .= phraseSuggesterHighlighterPostTag
      ]

instance FromJSON PhraseSuggesterHighlighter where
  parseJSON = withObject "PhraseSuggesterHighlighter" parse
    where
      parse o =
        PhraseSuggesterHighlighter
          <$> o .: "pre_tag"
          <*> o .: "post_tag"

data PhraseSuggesterCollate = PhraseSuggesterCollate
  { phraseSuggesterCollateTemplateQuery :: Query,
    phraseSuggesterCollateParams        :: TemplateQueryKeyValuePairs,
    phraseSuggesterCollatePrune         :: Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON PhraseSuggesterCollate where
  toJSON PhraseSuggesterCollate {..} =
    object
      [ "query"
          .= object
            [ "source" .= phraseSuggesterCollateTemplateQuery
            ],
        "params" .= phraseSuggesterCollateParams,
        "prune" .= phraseSuggesterCollatePrune
      ]

instance FromJSON PhraseSuggesterCollate where
  parseJSON (Object o) = do
    query' <- o .: "query"
    inline' <- query' .: "source"
    params' <- o .: "params"
    prune' <- o .:? "prune" .!= False
    return $ PhraseSuggesterCollate inline' params' prune'
  parseJSON x = typeMismatch "PhraseSuggesterCollate" x

data SuggestOptions = SuggestOptions
  { suggestOptionsText        :: Text,
    suggestOptionsScore       :: Double,
    suggestOptionsFreq        :: Maybe Int,
    suggestOptionsHighlighted :: Maybe Text
  }
  deriving (Eq, Read, Show, Generic)

instance ToJSON SuggestOptions where
  toJSON = genericToJSON defaultOptions

instance FromJSON SuggestOptions where
  parseJSON = withObject "SuggestOptions" parse
    where
      parse o =
        SuggestOptions
          <$> o .: "text"
          <*> o .: "score"
          <*> o .:? "freq"
          <*> o .:? "highlighted"

data SuggestResponse = SuggestResponse
  { suggestResponseText    :: Text,
    suggestResponseOffset  :: Int,
    suggestResponseLength  :: Int,
    suggestResponseOptions :: [SuggestOptions]
  }
  deriving (Eq, Read, Show, Generic)

instance ToJSON SuggestResponse where
  toJSON = genericToJSON defaultOptions

instance FromJSON SuggestResponse where
  parseJSON = withObject "SuggestResponse" parse
    where
      parse o =
        SuggestResponse
          <$> o .: "text"
          <*> o .: "offset"
          <*> o .: "length"
          <*> o .: "options"

data NamedSuggestionResponse = NamedSuggestionResponse
  { nsrName      :: Text,
    nsrResponses :: [SuggestResponse]
  }
  deriving (Eq, Read, Show, Generic)

instance ToJSON NamedSuggestionResponse where
  toJSON = genericToJSON defaultOptions

instance FromJSON NamedSuggestionResponse where
  parseJSON (Object o) = do
    suggestionName' <- case X.toList o of
      [(x, _)] -> return x
      _        -> fail "error parsing NamedSuggestionResponse name"
    suggestionResponses' <- o .: suggestionName'
    return $ NamedSuggestionResponse (toText suggestionName') suggestionResponses'
  parseJSON x = typeMismatch "NamedSuggestionResponse" x

data DirectGeneratorSuggestModeTypes
  = DirectGeneratorSuggestModeMissing
  | DirectGeneratorSuggestModePopular
  | DirectGeneratorSuggestModeAlways
  deriving (Eq, Show, Generic)

instance ToJSON DirectGeneratorSuggestModeTypes where
  toJSON DirectGeneratorSuggestModeMissing = "missing"
  toJSON DirectGeneratorSuggestModePopular = "popular"
  toJSON DirectGeneratorSuggestModeAlways  = "always"

instance FromJSON DirectGeneratorSuggestModeTypes where
  parseJSON = withText "DirectGeneratorSuggestModeTypes" parse
    where
      parse "missing" =
        pure DirectGeneratorSuggestModeMissing
      parse "popular" =
        pure DirectGeneratorSuggestModePopular
      parse "always" =
        pure DirectGeneratorSuggestModeAlways
      parse f =
        fail ("Unexpected DirectGeneratorSuggestModeTypes: " <> show f)

data DirectGenerators = DirectGenerators
  { directGeneratorsField         :: FieldName,
    directGeneratorsSize          :: Maybe Int,
    directGeneratorSuggestMode    :: DirectGeneratorSuggestModeTypes,
    directGeneratorMaxEdits       :: Maybe Double,
    directGeneratorPrefixLength   :: Maybe Int,
    directGeneratorMinWordLength  :: Maybe Int,
    directGeneratorMaxInspections :: Maybe Int,
    directGeneratorMinDocFreq     :: Maybe Double,
    directGeneratorMaxTermFreq    :: Maybe Double,
    directGeneratorPreFilter      :: Maybe Text,
    directGeneratorPostFilter     :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON DirectGenerators where
  toJSON DirectGenerators {..} =
    omitNulls
      [ "field" .= directGeneratorsField,
        "size" .= directGeneratorsSize,
        "suggest_mode" .= directGeneratorSuggestMode,
        "max_edits" .= directGeneratorMaxEdits,
        "prefix_length" .= directGeneratorPrefixLength,
        "min_word_length" .= directGeneratorMinWordLength,
        "max_inspections" .= directGeneratorMaxInspections,
        "min_doc_freq" .= directGeneratorMinDocFreq,
        "max_term_freq" .= directGeneratorMaxTermFreq,
        "pre_filter" .= directGeneratorPreFilter,
        "post_filter" .= directGeneratorPostFilter
      ]

instance FromJSON DirectGenerators where
  parseJSON = withObject "DirectGenerators" parse
    where
      parse o =
        DirectGenerators
          <$> o .: "field"
          <*> o .:? "size"
          <*> o .: "suggest_mode"
          <*> o .:? "max_edits"
          <*> o .:? "prefix_length"
          <*> o .:? "min_word_length"
          <*> o .:? "max_inspections"
          <*> o .:? "min_doc_freq"
          <*> o .:? "max_term_freq"
          <*> o .:? "pre_filter"
          <*> o .:? "post_filter"

mkDirectGenerators :: FieldName -> DirectGenerators
mkDirectGenerators fn =
  DirectGenerators
    fn
    Nothing
    DirectGeneratorSuggestModeMissing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
