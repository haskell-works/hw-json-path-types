module HaskellWorks.Data.Json.Path.Ast where

import Data.Int

data AstToken
  = AstTokenOfPathToken   PathToken
  | AstTokenOfFilterValue FilterValue

data FieldAccessor
  = RecursiveAnyField
  | AnyField
  | MultiField      [String]
  | RecursiveField  String
  | Field           String
  | RootNode

data ArrayAccessor
  = ArrayAccessorOfArrayRandomAccess  ArrayRandomAccess
  | ArrayAccessorOfArraySlice         ArraySlice

data ArrayRandomAccess = ArrayRandomAccess [Int]

data ArraySlice = ArraySlice
  { start :: Maybe Int
  , stop  :: Maybe Int
  , step  :: Int
  }

data Any = Null

data FilterValue
  = FilterValueOfFilterDirectValue  FilterDirectValue
  | FilterValueOfSubQuery           SubQuery
  | FilterValueOfJPString           JPString

data JPString = JPString String

data SubQuery = SubQuery [PathToken]

data FilterDirectValue
  = JPNull
  | FilterDirectValueOfJPNumber JPNumber
  | JPFalse
  | JPTrue

data JPNumber
  = JPLong    Int64
  | JPDouble  Double

data ComparisonOperator
  = EqOperator
  | NotEqOperator
  | LessOperator
  | GreaterOperator
  | LessOrEqOperator
  | GreaterOrEqOperator

data BinaryBooleanOperator
  = AndOperator
  | OrOperator

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

data RecursiveFilterToken = RecursiveFilterToken FilterToken

data PathToken
  = PathTokenOfFilterToken          FilterToken
  | CurrentNode
  | PathTokenOfArrayAccessor        ArrayAccessor
  | PathTokenOfFieldAccessor        FieldAccessor
  | PathTokenOfRecursiveFilterToken RecursiveFilterToken
