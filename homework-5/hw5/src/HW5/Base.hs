{-# LANGUAGE DeriveGeneric #-}

module HW5.Base
  ( HiValue (..)
  , HiFun (..)
  , HiExpr (..)
  , HiError (..)
  , HiAction (..)
  , HiMonad (..)
  ) where

import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

-- | Data type representing different types of values in the language.
data HiValue
  = HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Show, Eq, Ord, Generic)

instance Serialise HiValue

-- | Data type representing various functions available in the language.
data HiFun
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunIf
  | HiFunLength
  | HiFunToLower
  | HiFunToUpper
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Eq, Ord, Generic, Enum)

instance Serialise HiFun

instance Show HiFun where
  show HiFunDiv            = "div"
  show HiFunMul            = "mul"
  show HiFunAdd            = "add"
  show HiFunSub            = "sub"
  show HiFunNotLessThan    = "not-less-than"
  show HiFunNotGreaterThan = "not-greater-than"
  show HiFunNotEquals      = "not-equals"
  show HiFunNot            = "not"
  show HiFunAnd            = "and"
  show HiFunOr             = "or"
  show HiFunLessThan       = "less-than"
  show HiFunGreaterThan    = "greater-than"
  show HiFunEquals         = "equals"
  show HiFunIf             = "if"
  show HiFunLength         = "length"
  show HiFunToUpper        = "to-upper"
  show HiFunToLower        = "to-lower"
  show HiFunReverse        = "reverse"
  show HiFunTrim           = "trim"
  show HiFunList           = "list"
  show HiFunRange          = "range"
  show HiFunFold           = "fold"
  show HiFunPackBytes      = "pack-bytes"
  show HiFunUnpackBytes    = "unpack-bytes"
  show HiFunEncodeUtf8     = "encode-utf8"
  show HiFunDecodeUtf8     = "decode-utf8"
  show HiFunZip            = "zip"
  show HiFunUnzip          = "unzip"
  show HiFunSerialise      = "serialise"
  show HiFunDeserialise    = "deserialise"
  show HiFunRead           = "read"
  show HiFunWrite          = "write"
  show HiFunMkDir          = "mkdir"
  show HiFunChDir          = "cd"
  show HiFunParseTime      = "parse-time"
  show HiFunRand           = "rand"
  show HiFunEcho           = "echo"
  show HiFunCount          = "count"
  show HiFunKeys           = "keys"
  show HiFunValues         = "values"
  show HiFunInvert         = "invert"

-- | Data type representing expressions in the language.
data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show, Eq, Ord)

-- | Data type representing errors that can occur during evaluation.
data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Eq, Ord)

-- | Data type representing actions that can be performed.
data HiAction
  = HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Show, Eq, Ord, Generic)

instance Serialise HiAction

-- | Type class for monads that can run actions and return results.
class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
