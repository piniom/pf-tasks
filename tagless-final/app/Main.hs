{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad (forM, filterM)
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.Map as M
import qualified System.Directory as SD
import qualified System.FilePath as SF
import System.Environment (getArgs)

findToken :: MonadFS m => FilePath -> String -> m [(FilePath, Int, String)]
findToken dir token = do
  files <- listFiles dir
  fmap concat $ forM files $ \fp -> do
    ls <- readFileLines fp
    pure [ (fp, n, l)
         | (n, l) <- zip [1..] ls
         , token `elem` words l
         ]

class Monad m => MonadFS m where
  listFiles :: FilePath -> m [FilePath]
  readFileLines :: FilePath -> m [String]

  default listFiles :: (MonadTrans t, MonadFS m', m ~ t m') => FilePath -> m [FilePath]
  listFiles = lift . listFiles

  default readFileLines :: (MonadTrans t, MonadFS m', m ~ t m') => FilePath -> m [String]
  readFileLines = lift . readFileLines

instance MonadFS m => MonadFS (StateT s m)
instance MonadFS m => MonadFS (ReaderT r m)

type FileSystem = M.Map FilePath [String]

newtype TestApp a = TestApp { runTestApp :: StateT FileSystem Identity a }
  deriving (Functor, Applicative, Monad, MonadState FileSystem)

instance MonadFS TestApp where
  listFiles _ = gets M.keys
  readFileLines fp = gets (M.findWithDefault [] fp)


instance MonadFS IO where
  listFiles dir = do
    allEntries <- SD.listDirectory dir
    filterM (SD.doesFileExist . (dir SF.</>)) allEntries
  readFileLines = fmap lines . readFile

runCLI :: IO ()
runCLI = do
    args <- getArgs
    case args of
        [dir, token] -> do
            results <- findToken dir token
            mapM_ print results
        _ -> putStrLn "Usage: program <directory> <token>"

main :: IO ()
main = runCLI