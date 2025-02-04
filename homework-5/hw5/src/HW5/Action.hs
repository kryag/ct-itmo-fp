module HW5.Action
  ( HiPermission (..)
  , PermissionException (..)
  , HIO (..)
  ) where

import Control.Applicative (Alternative ((<|>)), optional)
import Control.Exception (Exception, throwIO)
import Control.Monad (ap, void)
import Data.ByteString as BS (readFile, writeFile)
import Data.Sequence as DS (fromList)
import Data.Set (Set, member)
import Data.Text as T (pack, unpack)
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Clock.POSIX (getCurrentTime)
import HW5.Base (HiAction (..), HiMonad (..), HiValue (..))
import System.Directory (createDirectory, getCurrentDirectory,
  listDirectory, setCurrentDirectory)
import System.Random.Stateful (getStdRandom, uniformR)

-- | Data type representing permissions
data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Exception type for when a required permission is not present.
newtype PermissionException = PermissionRequired HiPermission deriving (Eq, Ord)

instance Show PermissionException where
  show (PermissionRequired permission) = "Permission denied: " ++ show permission ++ " is required."

instance Exception PermissionException

-- | Newtype for a monad that runs actions with access to permissions.
newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance Functor HIO where
  fmap f a = HIO (fmap f . runHIO a)

instance Applicative HIO where
  pure a = HIO $ \_ -> pure a
  (<*>) = ap

instance Monad HIO where
  (>>=) a f = HIO $ \perms -> runHIO a perms >>= \result -> runHIO (f result) perms

instance HiMonad HIO where
  -- | Runs the 'HiActionRead' action, checking for 'AllowRead' permission.
  runAction (HiActionRead path) = needPermission AllowRead $
    readBytes <$> BS.readFile path <|> convertList <$> listDirectory path
      where
        readBytes bytes = case decodeUtf8' bytes of
          Left  _    -> HiValueBytes bytes
          Right text -> HiValueString text

        convertList paths = HiValueList (DS.fromList (map (HiValueString . T.pack) paths))

  -- | Runs the 'HiActionWrite' action, checking for 'AllowWrite' permission.
  runAction (HiActionWrite path bytes) = needPermission AllowWrite $ do
    BS.writeFile path bytes
    pure HiValueNull

  -- | Runs the 'HiActionMkDir' action, checking for 'AllowWrite' permission.
  runAction (HiActionMkDir path) = needPermission AllowWrite $ do
    void $ optional $ createDirectory path
    pure HiValueNull

  -- | Runs the 'HiActionChDir' action, checking for 'AllowRead' permission.
  runAction (HiActionChDir path) = needPermission AllowRead $ do
    setCurrentDirectory path
    pure HiValueNull

  -- | Runs the 'HiActionCwd' action, checking for 'AllowRead' permission.
  runAction HiActionCwd = needPermission AllowRead $ HiValueString . T.pack <$> getCurrentDirectory

  -- | Runs the 'HiActionNow' action, checking for 'AllowTime' permission.
  runAction HiActionNow = needPermission AllowTime $ HiValueTime <$> getCurrentTime

  -- | Runs the 'HiActionRand' action.
  runAction (HiActionRand lo hi) = HIO $ \_ -> HiValueNumber . toRational <$> getStdRandom (uniformR (lo, hi))

  -- | Runs the 'HiActionEcho' action, checking for 'AllowWrite' permission.
  runAction (HiActionEcho text) = needPermission AllowWrite $ do
    putStrLn $ T.unpack text
    pure HiValueNull

-- | Helper function to ensure that the required permission is present before running an action.
needPermission :: HiPermission -> IO HiValue -> HIO HiValue
needPermission permission action = HIO $ \permissions ->
  if member permission permissions
  then action
  else throwIO $ PermissionRequired permission
