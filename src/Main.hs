module Main (main) where

import Mail
import Opts
import Util
import Vmsg
import Walk
import Glob

import qualified Data.ByteString as B
import qualified Data.Map as M
import Control.Exception (handle, IOException)
import Control.Monad (foldM, foldM_, when, mplus)
import Control.Monad.Trans (MonadTrans (..), liftIO)
import Control.Monad.Reader (ask)
import Data.Bits (shiftL, (.&.))
import Data.Char (ord, toUpper)
import Data.Time (formatTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Directory (createDirectoryIfMissing,
                         doesDirectoryExist,
                         doesFileExist, removeFile)
import System.Environment (getArgs, getProgName)
import System.FilePath (splitExtension, takeExtension, (<.>), (</>))
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)


logWarning :: String -> ConfigReader IO ()
logWarning = whenVerbose . putStrLn . showString "WARNING: "

logInfo :: String -> ConfigReader IO ()
logInfo = whenVerbose . putStrLn . showString "INFO: "

whenVerbose :: Monad m => m () -> ConfigReader m ()
whenVerbose m = do
    cfg <- ask
    when (cfgVerbose cfg) (lift m)

q :: String -> String
q s = encode "CP932" s

newName :: FilePath -> IO FilePath
newName path = do
    exist <- doesFileExist path
    if exist
       then let (prefix, ext) = splitExtension path
            in loop prefix (2 :: Int) ext
       else return path
  where loop prefix i ext = do
            let path' = prefix ++ "(" ++ show i ++ ")" <.> ext
            exist <- doesFileExist path'
            if exist
               then loop prefix (i + 1) ext
               else return path'

handler :: IOException -> IO ()
handler _ = return ()

makePath :: FilePath -> IO ()
makePath path = handle handler (createDirectoryIfMissing True path)

type HashKey = (Int, Int, Int)

type Mbox = M.Map HashKey [FilePath]

enumerate :: [a] -> [(Int, Int, a)]
enumerate xs = zip3 [1..] (repeat (length xs)) xs

proc :: FilePath -> Mbox -> FilePath -> ConfigReader IO Mbox
proc dst mp src = do
    dir <- liftIO (doesDirectoryExist src)
    if dir
       then logWarning (q src ++ " is directory") >> return mp
       else logInfo ("process: " ++ q src) >>
           liftIO (B.readFile src) >>=
           foldM (dispatch dst src) mp . enumerate . parseVmsg

dispatch :: FilePath -> FilePath ->
    Mbox -> (Int, Int, Vmsg) -> ConfigReader IO Mbox
dispatch dst src mp (i, n, vmsg) =
    case vmsgIrmcType vmsg of
         Just INET -> writeMsg dst src mp (i, n, vmsg) "EML"
         Just SMS -> writeMsg dst src mp (i, n, vmsg) "SMS"
         _ -> return mp

writeMsg :: FilePath -> FilePath ->
    Mbox -> (Int, Int, Vmsg) -> String -> ConfigReader IO Mbox
writeMsg dst src mp (i, n, vmsg) ext = do
    let contents = vmsgContents vmsg
        folder = maybe "Unknown" (decode "CP932") (vmsgFolderName vmsg)
        parent = dst </> folder
        format = formatTime defaultTimeLocale "%Y%m%d%H%M%S"
    case parseMail contents of
         Just m
           | hash m `M.notMember` mp -> do
               path <- liftIO . newName $
                   parent </> maybe "unknown" format (mailDate m) <.> ext
               cfg <- ask
               when (cfgRunIO cfg) $ do
                   liftIO $ makePath parent
                   liftIO $ B.writeFile path contents
               logInfo $ printf "write: [%4d/%4d] %s" i n (q path)
               return (M.insertWith (++) (hash m) [path] mp)
           | otherwise -> return mp
         Nothing -> do
             logWarning $
                 printf "cannot parse message: [%4d/%4d] %s" i n (q src)
             return mp

hash :: Mail -> HashKey
hash m = (maybe 0 (truncate . utcTimeToPOSIXSeconds) (mailDate m),
          B.length (mailContents m),
          maybe 0 (foldl step 0) (mailFrom m `mplus` mailTo m))
  where step a i = (a `shiftL` 7 + ord i .&. 0x7f) `mod` 0x1ffffff

iter :: Mbox -> FilePath -> IO Mbox
iter mp path
  | map toUpper (takeExtension path) `elem` [".EML", ".SMS"] = do
      contents <- B.readFile path
      case parseMail contents of
           Just m -> return $ M.insertWith (++) (hash m) [path] mp
           Nothing -> return mp
  | otherwise = return mp

traverse :: FilePath -> IO Mbox
traverse path = walk iter M.empty path

clean :: Mbox -> ConfigReader IO ()
clean mp = mapM_ removeFile' (extractDups mp)
  where extractDups = concat . map init . M.elems . M.filter ((>= 2) . length)
        removeFile' file = do
            cfg <- ask
            when (cfgRunIO cfg) $ do
                liftIO $ removeFile file
                logInfo $ "remove: " ++ q file


main :: IO ()
main = do
    progName <- getProgName
    rawArgs <- getArgs
    case parseArgs rawArgs of
         Left err -> do
             putStrLn err
             putStrLn $ showUsage progName
         Right (cfg, args)
           | cfgClean cfg || (not . null) args -> do
               let root = cfgRoot cfg
               makePath root
               mbox <- traverse root
               flip runConfig cfg $ do
                   if null args
                      then logWarning "no inputs"
                      else liftIO (mapM glob args) >>=
                          foldM_ (proc root) mbox . concat
                   when (cfgClean cfg) (clean mbox)
           | otherwise -> do
               putStrLn ""
               putStrLn $ showUsage progName
