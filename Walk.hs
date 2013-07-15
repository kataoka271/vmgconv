module Walk (Iterator, walk) where

import Control.Monad.Trans (MonadIO (..), liftIO)
import System.Directory (getDirectoryContents,
                         getPermissions,
                         Permissions (..))
import System.FilePath ((</>))


type Iterator m a = a -> FilePath -> m a

walk :: MonadIO m => Iterator m a -> a -> FilePath -> m a
walk iter a root =
    liftIO (getDirectoryContents root) >>= \names ->
        fold a (filter (`notElem` [".", ".."]) names)
  where fold s (name:names) =
            let path = root </> name
            in liftIO (getPermissions path) >>= \perm ->
                if searchable perm
                   then walk iter s path >>= \s' -> fold s' names
                   else iter s path >>= \s' -> fold s' names
        fold s [] = return s
