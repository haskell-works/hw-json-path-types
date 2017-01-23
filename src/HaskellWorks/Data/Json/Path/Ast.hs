module HaskellWorks.Data.Json.Path.Ast where

import Data.Int

data AstToken
  = AstTokenOfPathToken   PathToken
  | AstTokenOfFilterValue FilterValue
  deriving (Eq, Show)

data FieldAccessor
  = RecursiveAnyField
  | AnyField
  | MultiField      [String]
  | RecursiveField  String
  | Field           String
  | RootNode
  deriving (Eq, Show)

data ArrayAccessor
  = ArrayAccessorOfArrayRandomAccess  ArrayRandomAccess
  | ArrayAccessorOfArraySlice         ArraySlice
  deriving (Eq, Show)

data ArrayRandomAccess = ArrayRandomAccess [Int]
  deriving (Eq, Show)

data ArraySlice = ArraySlice
  { start :: Maybe Int
  , stop  :: Maybe Int
  , step  :: Int
  }
  deriving (Eq, Show)

data Any = Null deriving (Eq, Show)

data FilterValue
  = FilterValueOfFilterDirectValue  FilterDirectValue
  | FilterValueOfSubQuery           SubQuery
  | FilterValueOfJPString           JPString
  deriving (Eq, Show)

data JPString = JPString String deriving (Eq, Show)

data SubQuery = SubQuery [PathToken] deriving (Eq, Show)

data FilterDirectValue
  = JPNull
  | FilterDirectValueOfJPNumber JPNumber
  | JPFalse
  | JPTrue
  deriving (Eq, Show)

data JPNumber
  = JPLong    Int64
  | JPDouble  Double
  deriving (Eq, Show)

data ComparisonOperator
  = EqOperator
  | NotEqOperator
  | LessOperator
  | GreaterOperator
  | LessOrEqOperator
  | GreaterOrEqOperator
  deriving (Eq, Show)

data BinaryBooleanOperator
  = AndOperator
  | OrOperator
  deriving (Eq, Show)

data FilterToken
  = BooleanFilter
    { booleanOperator   :: BinaryBooleanOperator
    , booleanFilterLhs  :: FilterToken
    , booleanFilterRhs  :: FilterToken
    }
  | ComparisonFilter
    { operator          :: ComparisonOperator
    , comparisonLhs     :: FilterValue
    , comparisonRhs     :: FilterValue
    }
  | HasFilter SubQuery
  deriving (Eq, Show)

data RecursiveFilterToken = RecursiveFilterToken FilterToken
  deriving (Eq, Show)

data PathToken
  = PathTokenOfFilterToken          FilterToken
  | CurrentNode
  | PathTokenOfArrayAccessor        ArrayAccessor
  | PathTokenOfFieldAccessor        FieldAccessor
  | PathTokenOfRecursiveFilterToken RecursiveFilterToken
  deriving (Eq, Show)
