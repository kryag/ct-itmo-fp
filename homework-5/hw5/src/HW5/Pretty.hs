{-# LANGUAGE OverloadedStrings #-}

module HW5.Pretty
  ( prettyValue
  ) where

import Data.ByteString (ByteString, unpack)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Ratio (numerator, denominator)
import Data.Scientific (FPFormat (Fixed), formatScientific, fromRationalRepetendUnlimited)
import Numeric (showHex)
import HW5.Base (HiAction(..), HiValue(..))
import Prettyprinter (Doc, Pretty (pretty), encloseSep, viaShow, slash, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)

-- | Pretty-print a 'HiValue' as a 'Doc' with ANSI styling.
-- Handles different 'HiValue' types such as numbers, strings, lists, dictionaries, etc.
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber r)   = prettyNumber r
prettyValue (HiValueFunction f) = viaShow f
prettyValue (HiValueBool True)  = "true"
prettyValue (HiValueBool False) = "false"
prettyValue HiValueNull         = "null"
prettyValue (HiValueString s)   = viaShow s
prettyValue (HiValueList l)     = prettyList l
prettyValue (HiValueBytes b)    = prettyBytes b
prettyValue (HiValueAction a)   = prettyAction a
prettyValue (HiValueTime time)  = "parse-time(\"" <> viaShow time <> "\")"
prettyValue (HiValueDict d)     = prettyDict d

-- | Pretty-print a 'Rational' number as a 'Doc'.
-- Displays the number in a simplified or scientific format, based on its value.
prettyNumber :: Rational -> Doc AnsiStyle
prettyNumber r =
  let num = numerator r
      den = denominator r
  in if den == 1
       then pretty $ show num
       else case fromRationalRepetendUnlimited r of
         (scientific, Nothing) -> pretty $ formatScientific Fixed Nothing scientific
         (_, Just _) ->
           let (whole, frac) = quotRem num den
           in if whole /= 0
                then pretty whole <+> (if whole > 0 then "+" else "-") <+> prettyFraction (abs frac) den
                else prettyFraction num den

-- | Pretty-print a fraction as a 'Doc'.
prettyFraction :: Integer -> Integer -> Doc AnsiStyle
prettyFraction num denom = pretty num <> slash <> pretty denom

-- | Pretty-print a list of 'HiValue' items as a 'Doc'.
prettyList :: Foldable t => t HiValue -> Doc AnsiStyle
prettyList l =
  if null (toList l)
    then "[ ]"
    else encloseSep "[ " " ]" ", " (map prettyValue $ toList l)

-- | Pretty-print a 'ByteString' as a 'Doc'.
prettyBytes :: ByteString -> Doc AnsiStyle
prettyBytes b =
  if null (unpack b)
    then "[# #]"
    else encloseSep "[# " " #]" " " (map prettyByte $ unpack b)
  where
    prettyByte byte = pretty $ formatHex (showHex byte "")
    formatHex hex = if length hex == 1 then '0' : hex else hex

-- | Pretty-print a 'HiAction' as a 'Doc'.
prettyAction :: HiAction -> Doc AnsiStyle
prettyAction (HiActionRead path)        = "read(" <> viaShow path <> ")"
prettyAction (HiActionWrite path bytes) = "write(" <> viaShow path <> "," <+> prettyBytes bytes <> ")"
prettyAction (HiActionMkDir path)       = "mkdir(" <> viaShow path <> ")"
prettyAction (HiActionChDir path)       = "cd(" <> viaShow path <> ")"
prettyAction HiActionCwd                = "cwd"
prettyAction HiActionNow                = "now"
prettyAction (HiActionRand lo hi)       = "rand(" <> pretty lo <> "," <+> pretty hi <> ")"
prettyAction (HiActionEcho text)        = "echo(" <> viaShow text <> ")"

-- | Pretty-print a dictionary of 'HiValue' key-value pairs as a 'Doc'.
prettyDict :: M.Map HiValue HiValue -> Doc AnsiStyle
prettyDict d =
  if null (M.toList d)
    then "{ }"
    else encloseSep "{ " " }" ", " (map prettyElem $ M.toList d)
  where
    prettyElem (k, v) = prettyValue k <> ":" <+> prettyValue v
