{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

module HW5.Evaluator (
    eval
  ) where

import Control.Monad (foldM)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Bitraversable (bimapM)
import Data.Foldable (Foldable (toList))
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (Semigroup (stimes))
import Data.Time (addUTCTime, diffUTCTime)
import Data.Word (Word8)
import Codec.Serialise(deserialise, serialise)
import Codec.Compression.Zlib (CompressParams (compressLevel), bestCompression, compressWith,
  decompress, defaultCompressParams)
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Text.Read as TR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Sequence as DS
import HW5.Base (HiValue (..), HiFun (..), HiExpr (..), HiError (..), HiAction (..), HiMonad (..))

-- | Evaluates a given Hi expression.
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT $ eval' expr

-- | The internal function that recursively evaluates expressions.
eval' :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
eval' (HiExprValue val)    = return val
eval' (HiExprDict dict)    = HiValueDict . Map.fromList <$> mapM (bimapM eval' eval') dict
eval' (HiExprApply h t)    = handleApply =<< eval' h
  where
    handleApply (HiValueFunction fun) =
      if checkArity fun (length t)
      then tryLazyEval fun t
      else throwE HiErrorArityMismatch
    handleApply (HiValueDict dict) =
      if length t == 1
      then eval' (head t) >>= \arg -> return (fromMaybe HiValueNull (Map.lookup arg dict))
      else throwE HiErrorInvalidArgument
    handleApply str@(HiValueString _)   = sliceableEvaluation str t
    handleApply list@(HiValueList _)    = sliceableEvaluation list t
    handleApply bytes@(HiValueBytes _)  = sliceableEvaluation bytes t
    handleApply _                       = throwE HiErrorInvalidFunction

eval' (HiExprRun runnable) = handleRun =<< eval' runnable
  where
    handleRun (HiValueAction action) = lift $ runAction action
    handleRun _                      = throwE HiErrorInvalidArgument

-- | Tries to evaluate a function with the provided arguments in a lazy manner.
tryLazyEval :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
tryLazyEval HiFunAnd [left, right] = do
  leftResult <- eval' left
  if | leftResult == HiValueNull       -> return HiValueNull
     | leftResult == HiValueBool False -> return $ HiValueBool False
     | otherwise                       -> eval' right

tryLazyEval HiFunOr [left, right] = do
  leftResult <- eval' left
  if | leftResult == HiValueNull       -> eval' right
     | leftResult == HiValueBool False -> eval' right
     | otherwise                       -> return leftResult

tryLazyEval HiFunIf [cond, trueBranch, falseBranch] = do
  condResult <- eval' cond
  if | condResult == HiValueBool True  -> eval' trueBranch
     | condResult == HiValueBool False -> eval' falseBranch
     | otherwise                       -> throwE HiErrorInvalidArgument

tryLazyEval function arguments = evalF function =<< mapM eval' arguments

-- | Evaluates a function with the given arguments.
evalF :: HiMonad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
evalF HiFunDiv [HiValueNumber x, HiValueNumber y] =
  if y == 0
  then throwE HiErrorDivideByZero
  else return $ HiValueNumber $ x / y
evalF HiFunDiv [HiValueString x, HiValueString y] = return $ HiValueString $ DT.concat [x, DT.pack "/", y]

evalF HiFunMul [HiValueNumber x, HiValueNumber y]       = return $ HiValueNumber $ x * y
evalF HiFunMul [HiValueString str, HiValueNumber num]   = return $ HiValueString $ stimes (numerator num) str
evalF HiFunMul [HiValueList list, HiValueNumber num]    = return $ HiValueList $ stimes (numerator num) list
evalF HiFunMul [HiValueBytes bytes, HiValueNumber num]  = return $ HiValueBytes $ stimes (numerator num) bytes

evalF HiFunAdd [HiValueNumber x, HiValueNumber y]      = return $ HiValueNumber $ x + y
evalF HiFunAdd [HiValueString x, HiValueString y]      = return $ HiValueString $ x <> y
evalF HiFunAdd [HiValueList x, HiValueList y]          = return $ HiValueList $ x <> y
evalF HiFunAdd [HiValueBytes x, HiValueBytes y]        = return $ HiValueBytes $ x <> y
evalF HiFunAdd [HiValueTime time, HiValueNumber num]   = return $ HiValueTime $ addUTCTime (fromIntegral $ numerator num) time

evalF HiFunSub [HiValueNumber x, HiValueNumber y] = return $ HiValueNumber $ x - y
evalF HiFunSub [HiValueTime x, HiValueTime y]     = return $ HiValueNumber $ toRational $ diffUTCTime x y

evalF HiFunLessThan [x, y]       = return $ HiValueBool $ x < y
evalF HiFunGreaterThan [x, y]    = return $ HiValueBool $ x > y
evalF HiFunNotLessThan [x, y]    = return $ HiValueBool $ x >= y
evalF HiFunNotGreaterThan [x, y] = return $ HiValueBool $ x <= y
evalF HiFunEquals [x, y]         = return $ HiValueBool $ x == y
evalF HiFunNotEquals [x, y]      = return $ HiValueBool $ x /= y

evalF HiFunNot [HiValueBool x] = return $ HiValueBool $ not x

evalF HiFunIf [HiValueBool True, x, _]  = return x
evalF HiFunIf [HiValueBool False, _, y] = return y

evalF HiFunAnd [HiValueBool x, HiValueBool y] = return $ HiValueBool $ x && y
evalF HiFunOr [HiValueBool x, HiValueBool y]  = return $ HiValueBool $ x || y

evalF HiFunLength [HiValueString x] = return $ HiValueNumber $ toRational $ DT.length x
evalF HiFunLength [HiValueList x]   = return $ HiValueNumber $ toRational $ DS.length x
evalF HiFunLength [HiValueBytes x]  = return $ HiValueNumber $ toRational $ BS.length x

evalF HiFunToUpper [HiValueString x] = return $ HiValueString $ DT.toUpper x
evalF HiFunToLower [HiValueString x] = return $ HiValueString $ DT.toLower x

evalF HiFunReverse [HiValueString x] = return $ HiValueString $ DT.reverse x
evalF HiFunReverse [HiValueList x]   = return $ HiValueList $ DS.reverse x

evalF HiFunTrim [HiValueString x] = return $ HiValueString $ DT.strip x

evalF HiFunList args = return $ HiValueList $ DS.fromList args

evalF HiFunRange [HiValueNumber x, HiValueNumber y] = return $ HiValueList $ DS.fromList $ HiValueNumber <$> [x .. y]

evalF HiFunFold [HiValueFunction fun, HiValueList elements]
  | not (checkArity fun 2) = throwE HiErrorArityMismatch
  | DS.null elements       = return HiValueNull
  | otherwise = do
      let (first, other) = (DS.index elements 0, DS.drop 1 elements)
      foldM (\acc cur -> evalF fun [acc, cur]) first other

evalF HiFunPackBytes [HiValueList elements] = do
  byteList <- traverse getByte (toList elements)
  return $ HiValueBytes $ BS.pack byteList
  where
    getByte (HiValueNumber num)
      | denominator num == 1 && 0 <= numerator num && numerator num < 256 =
          return (fromIntegral $ numerator num :: Word8)
      | otherwise = throwE HiErrorInvalidArgument
    getByte _ = throwE HiErrorInvalidArgument

evalF HiFunUnpackBytes [HiValueBytes bytes] = return $ HiValueList $ DS.fromList $
  HiValueNumber . (toRational :: Int -> Rational) . fromIntegral <$> BS.unpack bytes

evalF HiFunEncodeUtf8 [HiValueString str]   = return $ HiValueBytes $ DTE.encodeUtf8 str
evalF HiFunDecodeUtf8 [HiValueBytes bytes]  = return $ either (const HiValueNull) HiValueString $ DTE.decodeUtf8' bytes

evalF HiFunZip [HiValueBytes bytes]   = return $ HiValueBytes $ BSL.toStrict $
  compressWith defaultCompressParams { compressLevel = bestCompression } $ BSL.fromStrict bytes
evalF HiFunUnzip [HiValueBytes bytes] = return $ HiValueBytes $ BSL.toStrict $ decompress $ BSL.fromStrict bytes

evalF HiFunSerialise [x]                    = return $ HiValueBytes $ BSL.toStrict $ serialise x
evalF HiFunDeserialise [HiValueBytes bytes] = return $ deserialise (BSL.fromStrict bytes)

evalF HiFunRead [HiValueString p]                   = return $ HiValueAction $ HiActionRead $ DT.unpack p
evalF HiFunWrite [HiValueString p, HiValueString t] = return $ HiValueAction $ HiActionWrite (DT.unpack p) $ DTE.encodeUtf8 t
evalF HiFunMkDir [HiValueString p]                  = return $ HiValueAction $ HiActionMkDir $ DT.unpack p
evalF HiFunChDir [HiValueString p]                  = return $ HiValueAction $ HiActionChDir $ DT.unpack p
evalF HiFunParseTime [HiValueString s]              = return $ maybe HiValueNull HiValueTime $ TR.readMaybe $ DT.unpack s
evalF HiFunEcho [HiValueString s]                   = return $ HiValueAction $ HiActionEcho s

evalF HiFunRand [HiValueNumber lo, HiValueNumber hi] =
  if denominator lo == 1 && denominator hi == 1
  then return $ HiValueAction $ HiActionRand (truncate lo) (truncate hi)
  else throwE HiErrorInvalidArgument

evalF HiFunKeys [HiValueDict dict]    = return $ HiValueList $ DS.fromList $ Map.keys dict
evalF HiFunValues [HiValueDict dict]  = return $ HiValueList $ DS.fromList $ Map.elems dict

evalF HiFunInvert [HiValueDict dict]  = return $ HiValueDict $ Map.map (HiValueList . DS.fromList)
  $ Map.fromListWith (++) $ fmap (\(k, v) -> (v, [k])) (Map.toList dict)

evalF HiFunCount [HiValueList list]    = countOccurrences list
evalF HiFunCount [HiValueBytes bytes]  = countOccurrences bytes
evalF HiFunCount [HiValueString str]   = countOccurrences str

evalF _ _ = throwE HiErrorInvalidArgument

-- | Counts the occurrences of elements in a slice.
countOccurrences :: (Sliceable a, HiMonad m) => a -> ExceptT HiError m HiValue
countOccurrences slice = do
  let elems = convertToHi slice
      countedMap = Map.fromListWith (+) [(e, 1) | e <- elems]
  return . HiValueDict $ Map.map (HiValueNumber . (toRational :: Int -> Rational)) countedMap

-- | Evaluates sliceable operations based on provided arguments.
evalSliceable :: (HiMonad m, Sliceable a) => [HiValue] -> a -> (a -> HiValue) -> ExceptT HiError m HiValue
evalSliceable arguments container constructor =
  case normalizeSliceable arguments (length $ convertToHi container) of
    Just (start, end) -> return $ constructor $ convertFromHi $ take (end - start) $ drop start $ convertToHi container
    Nothing           -> throwE HiErrorInvalidArgument

-- | Normalizes sliceable operations to ensure valid index range.
normalizeSliceable :: [HiValue] -> Int -> Maybe (Int, Int)
normalizeSliceable [HiValueNumber a, HiValueNumber b] len = Just (adjustIndex a len, adjustIndex b len)
normalizeSliceable [HiValueNumber a, HiValueNull] len     = Just (adjustIndex a len, len)
normalizeSliceable [HiValueNull, HiValueNumber b] len     = Just (0, adjustIndex b len)
normalizeSliceable [HiValueNull, HiValueNull] len         = Just (0, len)
normalizeSliceable _ _                                    = Nothing

-- | Adjusts an index based on its position and the length of the slice.
adjustIndex :: Rational -> Int -> Int
adjustIndex r len = let idx = truncate r in if idx < 0 then len + idx else idx

-- | Retrieves an element from a sliceable object by its index.
getSliceableElement :: (HiMonad m, Sliceable a) => HiValue -> a -> ExceptT HiError m HiValue
getSliceableElement (HiValueNumber idx) container =
  let elems = convertToHi container
      index = truncate idx
  in if index < 0 || index >= length elems
     then return HiValueNull
     else return $ elems !! index
getSliceableElement _ _                           = throwE HiErrorInvalidArgument

-- | Applies a sliceable operation based on the number of arguments provided.
applySliceableOperation :: (HiMonad m, Sliceable a) => (a -> HiValue) -> a -> [HiExpr] -> ExceptT HiError m HiValue
applySliceableOperation constructor container args
  | length args == 1 = eval' (head args) >>= \index -> getSliceableElement index container
  | length args == 2 = mapM eval' args >>= \slice -> evalSliceable slice container constructor
  | otherwise        = throwE HiErrorArityMismatch

-- | Evaluates sliceable objects such as strings, lists and byte arrays.
sliceableEvaluation :: (HiMonad m) => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
sliceableEvaluation (HiValueString str)   = applySliceableOperation HiValueString str
sliceableEvaluation (HiValueList list)    = applySliceableOperation HiValueList list
sliceableEvaluation (HiValueBytes bytes)  = applySliceableOperation HiValueBytes bytes
sliceableEvaluation _                     = \_ -> throwE HiErrorInvalidArgument

-- | A class for objects that can be indexing.
class Sliceable a where
  convertToHi :: a -> [HiValue]
  convertFromHi :: [HiValue] -> a

instance Sliceable DT.Text where
  convertToHi text     = HiValueString . DT.singleton <$> DT.unpack text
  convertFromHi values = DT.concat (convert <$> values)
    where
      convert :: HiValue -> DT.Text
      convert (HiValueString text) = text
      convert _                    = error "Invalid type, expected HiValueString"

instance Sliceable (DS.Seq HiValue) where
  convertToHi   = toList
  convertFromHi = DS.fromList

instance Sliceable BS.ByteString where
  convertToHi bytes    = HiValueNumber . toRational <$> BS.unpack bytes
  convertFromHi values = BS.pack (convert <$> values)
    where
      convert :: HiValue -> Word8
      convert (HiValueNumber num) = fromIntegral $ numerator num
      convert  _                  = error "Invalid type, expected HiValueNumber"

-- | Checks if the actual number of arguments matches the expected number for a function.
checkArity :: HiFun -> Int -> Bool
checkArity fun actualArgsCount =
  let expectedArgsCount = getFunArity fun
  in expectedArgsCount == actualArgsCount || expectedArgsCount == -1

-- | Returns the expected number of arguments for a given function.
getFunArity :: HiFun -> Int
getFunArity HiFunDiv            = 2
getFunArity HiFunMul            = 2
getFunArity HiFunAdd            = 2
getFunArity HiFunSub            = 2
getFunArity HiFunNot            = 1
getFunArity HiFunAnd            = 2
getFunArity HiFunOr             = 2
getFunArity HiFunLessThan       = 2
getFunArity HiFunGreaterThan    = 2
getFunArity HiFunEquals         = 2
getFunArity HiFunNotLessThan    = 2
getFunArity HiFunNotGreaterThan = 2
getFunArity HiFunNotEquals      = 2
getFunArity HiFunIf             = 3
getFunArity HiFunLength         = 1
getFunArity HiFunToUpper        = 1
getFunArity HiFunToLower        = 1
getFunArity HiFunReverse        = 1
getFunArity HiFunTrim           = 1
getFunArity HiFunList           = -1 -- arbitrary number of arguments
getFunArity HiFunRange          = 2
getFunArity HiFunFold           = 2
getFunArity HiFunPackBytes      = 1
getFunArity HiFunUnpackBytes    = 1
getFunArity HiFunEncodeUtf8     = 1
getFunArity HiFunDecodeUtf8     = 1
getFunArity HiFunZip            = 1
getFunArity HiFunUnzip          = 1
getFunArity HiFunSerialise      = 1
getFunArity HiFunDeserialise    = 1
getFunArity HiFunRead           = 1
getFunArity HiFunWrite          = 2
getFunArity HiFunMkDir          = 1
getFunArity HiFunChDir          = 1
getFunArity HiFunParseTime      = 1
getFunArity HiFunRand           = 2
getFunArity HiFunEcho           = 1
getFunArity HiFunCount          = 1
getFunArity HiFunKeys           = 1
getFunArity HiFunValues         = 1
getFunArity HiFunInvert         = 1
