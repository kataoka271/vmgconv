{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Opts (
      ConfigReader
    , runConfig
    , Config (..)
    , parseArgs
    , showUsage
  ) where

import System.Console.GetOpt
import Control.Monad.Reader

newtype ConfigReader m a = ConfigReader { unwrap :: ReaderT Config m a }
  deriving (Functor, Monad, MonadIO, MonadTrans, MonadReader Config)

runConfig :: ConfigReader m a -> Config -> m a
runConfig = runReaderT . unwrap

data Config = Config {
    cfgVerbose :: Bool,
    cfgRoot :: FilePath,
    cfgClean :: Bool
  } deriving (Show)

defaultConfig :: Config
defaultConfig = Config {
    cfgVerbose = True,
    cfgRoot = "MailBox",
    cfgClean = False
  }

options :: [OptDescr (Config -> Maybe Config) ]
options = [
      Option "q" ["quiet"] (NoArg opt_quiet)
        "disable verbose output"
    , Option "r" ["root"] (ReqArg opt_root "DIR")
        "set root directory"
    , Option "C" ["clean"] (NoArg opt_clean)
        "clean duplicate files after processing"
    , Option "h" ["help"] (NoArg opt_usage)
        "print this help"
    ]
  where
    opt_quiet cfg = return cfg { cfgVerbose = False }
    opt_root arg cfg = return cfg { cfgRoot = arg }
    opt_clean cfg = return cfg { cfgClean = True }
    opt_usage _ = Nothing

parseArgs :: [String] -> Either String (Config, [String])
parseArgs rawArgs =
    case getOpt RequireOrder options rawArgs of
         (acts, args, []) ->
             case foldl (>>=) (return defaultConfig) acts of
                  Just cfg -> Right (cfg, args)
                  Nothing -> Left ""
         (_, _, errs) -> Left $ concat errs

showUsage :: String -> String
showUsage progName =
    usageInfo ("usage: " ++ progName ++ " [options] filenames") options
