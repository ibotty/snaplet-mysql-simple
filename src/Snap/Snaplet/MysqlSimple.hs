{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

{-|

This snaplet makes it simple to use a MariaDB or MySQL database from your Snap
application and is a literal translation of snaplet-postgresql-simple by
Doug Beardsley (<https://github.com/mightybyte/snaplet-postgresql-simple>).
It uses the excellent mysql-simple library
(<http://hackage.haskell.org/package/mysql-simple>) by Bryan O\'Sullivan.
Now, adding a database to your web app takes just two simple steps.

First, include this snaplet in your application's state.

> data App = App
>     { ... -- Other state needed in your app
>     , _db :: Snaplet Mysql
>     }

Next, call the mysqlInit from your application's initializer.

> appInit = makeSnaplet ... $ do
>     ...
>     d <- nestSnaplet "db" db mysqlInit
>     return $ App ... d

Now you can use any of the mysql-simple wrapper functions defined in this
module anywhere in your application handlers.  For instance:

> postHandler :: Handler App App ()
> postHandler = do
>     posts <- with db $ query_ "select * from blog_post"
>     ...

Optionally, if you find yourself doing many database queries, you can eliminate some of the boilerplate by defining a HasMysql instance for your application.

> instance HasMysql (Handler b App) where
>   getMysqlState = with db get

With this code, our postHandler example no longer requires the 'with' function:

> postHandler :: Handler App App ()
> postHandler = do
>     posts <- query_ "select * from blog_post"
>     ...

The first time you run an application with the mysql-simple snaplet,
a configuration file @devel.cfg@ is created in the
@snaplets/mysql-simple@ directory underneath your project root.  It
specifies how to connect to your MySQL or MariaDB server and what user,
password, and database to use.  Edit this file and modify the values
appropriately and you'll be off and running.

If you want to have out-of-the-box authentication, look at the documentation
for the "Snap.Snaplet.Auth.Backends.MysqlSimple" module.

-}

module Snap.Snaplet.MysqlSimple (
  -- * The Snaplet
    Mysql(..)
  , HasMysql(..)
  , mysqlInit
  , mysqlInit'
  , getConnectionInfo

  -- * Wrappers and re-exports
  , query
  , query_
  , fold
  , fold_
  , forEach
  , forEach_
  , execute
  , execute_
  , executeMany
  , rollback
  , commit
  , withTransaction
  , formatMany
  , formatQuery

  -- Re-exported from mysql-simple
  , M.Query
  , M.In(..)
  , M.Binary(..)
  , M.Only(..)
  , M.FormatError(..)
  , M.QueryError(..)
  , M.ResultError(..)
  , MB.MySQLError(..)
  , QueryResults(..)
  , QueryParams(..)

  , M.defaultConnectInfo
  ) where

import Debug.Trace

import           Prelude hiding ((++))
import           Control.Lens -- (ASetter(), camelCaseFields, makeLensesWith, set)
import           Control.Monad.CatchIO (MonadCatchIO)
import qualified Control.Monad.CatchIO as CIO
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Reader
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Char
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import           Data.Int
import           Data.Maybe
import           Data.Monoid(Monoid(..))
import           Data.Pool
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Database.MySQL.Simple as M
import qualified Database.MySQL.Base as MB
import           Database.MySQL.Simple.QueryParams
import           Database.MySQL.Simple.QueryResults
import           Snap
import           Paths_snaplet_mysql_simple


-- This is actually more portable than using <>
(++) :: Monoid a => a -> a -> a
(++) = mappend
infixr 5 ++

------------------------------------------------------------------------------
-- | The state for the mysql-simple snaplet. To use it in your app
-- include this in your application state and use pgsInit to initialize it.
data Mysql = Mysql
    { mysqlPool :: Pool M.Connection
    -- ^ Function for retrieving the connection pool
    }


------------------------------------------------------------------------------
-- | Instantiate this typeclass on 'Handler b YourAppState' so this snaplet
-- can find the connection source.  If you need to have multiple instances of
-- the mysql snaplet in your application, then don't provide this instance
-- and leverage the default instance by using \"@with dbLens@\" in front of calls
-- to snaplet-mysql-simple functions.
class (MonadCatchIO m) => HasMysql m where
    getMysqlState :: m Mysql


------------------------------------------------------------------------------
-- | Default instance
instance HasMysql (Handler b Mysql) where
    getMysqlState = get


------------------------------------------------------------------------------
-- | A convenience instance to make it easier to use this snaplet in the
-- Initializer monad like this:
--
-- > d <- nestSnaplet "db" db pgsInit
-- > count <- liftIO $ runReaderT (execute "INSERT ..." params) d
instance (MonadCatchIO m) => HasMysql (ReaderT (Snaplet Mysql) m) where
    getMysqlState = asks (^# snapletValue)


------------------------------------------------------------------------------
-- | A convenience instance to make it easier to use functions written for
-- this snaplet in non-snaplet contexts.
instance (MonadCatchIO m) => HasMysql (ReaderT Mysql m) where
    getMysqlState = ask


------------------------------------------------------------------------------
-- | Orphan Lenses for ConnectInfo
--   Not exported, only used for 'getConnectionInfo'.
$(makeLensesWith (LensRules Just Just (const Nothing)
                     (S.fromList [SimpleLenses, GenerateSignatures]))
                 ''M.ConnectInfo)
$(makeLensesWith (LensRules Just Just (const Nothing)
                     (S.fromList [SimpleLenses, GenerateSignatures]))
                 ''MB.SSLInfo)

------------------------------------------------------------------------------
-- | Produce a connection info from a config
getConnectionInfo :: MonadIO m => C.Config -> m M.ConnectInfo
getConnectionInfo config = do
    sslOpts <- foldl modRecord (return MB.defaultSSLInfo) sslParams
    connOpts <- liftM catMaybes $ mapM handleOpts optParams

    connInfo <- foldl modRecord (return M.defaultConnectInfo) params

    trace (show connInfo) $ return $ set connectSSL (Just sslOpts) $ set connectOptions connOpts connInfo
  where params =
            [ ("host", set connectHost)
            , ("port", set connectPort . read)
            , ("dbname", set connectDatabase)
            , ("user", set connectUser)
            , ("password", set connectPassword)
            , ("path", set connectPath)
            ]
        optParams =
            [ ("connection_timeout", MB.ConnectTimeout . read)
            , ("compress", const MB.Compress)
            , ("named_pipe", const MB.NamedPipe)
            , ("init_commmand", MB.InitCommand . B8.pack)
            , ("read_default_file", MB.ReadDefaultFile)
            , ("read_default_group", MB.ReadDefaultGroup . B8.pack)
            , ("charset_dir", MB.CharsetDir)
            , ("charset_name", MB.CharsetName)
            , ("local_in_file", MB.LocalInFile . readBool)
            , ("protocol", \s -> MB.Protocol $ case map toLower s of
                                    "tcp" -> MB.TCP
                                    "socket" -> MB.Socket
                                    "pipe" -> MB.Pipe
                                    "memory" -> MB.Memory
                                    _ -> error "no valid protocol.")
            , ("shared_memory_base_name", MB.SharedMemoryBaseName . B8.pack)
            , ("read_timeout", MB.ReadTimeout . read)
            , ("write_timeout", MB.WriteTimeout . read)
            , ("use_remote_connection", const MB.UseRemoteConnection)
            , ("use_embedded_connection", const MB.UseEmbeddedConnection)
            , ("guess_connection", const MB.GuessConnection)
            , ("client_ip", MB.ClientIP . B8.pack)
            , ("secure_auth", MB.SecureAuth . readBool)
            , ("report_data_truncation", MB.ReportDataTruncation . readBool)
            , ("reconnect", MB.Reconnect . readBool)
            , ("ssl_verify_server_cert", MB.SSLVerifyServerCert . readBool)
            , ("found_rows", const MB.FoundRows)
            , ("ignore_sigpipe", const MB.IgnoreSIGPIPE)
            , ("ignore_space", const MB.IgnoreSpace)
            , ("interactive", const MB.Interactive)
            , ("local_files", const MB.LocalFiles)
            , ("multi_results", const MB.MultiResults)
            , ("multi_statements", const MB.MultiStatements)
            , ("no_schema", const MB.NoSchema)
            ]
        sslParams =
            [ ("ssl_key", set sslKey)
            , ("ssl_cert", set sslCert)
            , ("ssl_ca", set sslCA)
            , ("ssl_ca_path", set sslCAPath)
            , ("ssl_ciphers", set sslCiphers)
            ]
        modRecord :: MonadIO m => m a -> (C.Name, String -> a -> a) -> m a
        modRecord conf (name, setter) = do
            x <- liftIO $ C.lookup config name
            case x of
                Just val -> liftM (setter val) conf
                Nothing  -> conf
        handleOpts :: MonadIO m => (C.Name, String -> a) -> m (Maybe a)
        handleOpts (name, f) = liftIO $ liftM (fmap f) $ C.lookup config name

        readBool :: String -> Bool
        readBool s = case map toLower s of
                         "yes" -> True
                         "y" -> True
                         "true" -> True
                         "no" -> False
                         "n" -> False
                         "false" -> False
                         _ -> error "expected 'yes' or 'no'."

description :: Text
description = "MySQL abstraction"

datadir :: Maybe (IO FilePath)
datadir = Just $ liftM (++"/resources/db") getDataDir


------------------------------------------------------------------------------
-- | Initialize the snaplet
mysqlInit :: SnapletInit b Mysql
mysqlInit = makeSnaplet "mysql-simple" description datadir $ do
    config <- getSnapletUserConfig
    initHelper config


------------------------------------------------------------------------------
-- | Initialize the snaplet
mysqlInit' :: C.Config -> SnapletInit b Mysql
mysqlInit' config = makeSnaplet "mysql-simple" description datadir $
    initHelper config


initHelper :: MonadIO m => C.Config -> m Mysql
initHelper config = do
    conninfo <- liftIO $ getConnectionInfo config
    stripes <- liftIO $ C.lookupDefault 1 config "numStripes"
    idle <- liftIO $ C.lookupDefault 5 config "idleTime"
    resources <- liftIO $ C.lookupDefault 20 config "maxResourcesPerStripe"
    pool <- liftIO $ createPool (M.connect conninfo) M.close stripes
                                (realToFrac (idle :: Double)) resources
    return $ Mysql pool


------------------------------------------------------------------------------
-- | Convenience function for executing a function that needs a database
-- connection.
withMysql :: (HasMysql m)
       => (M.Connection -> IO b) -> m b
withMysql f = do
    s <- getMysqlState
    let pool = mysqlPool s
    liftIO $ withResource pool f


------------------------------------------------------------------------------
-- | See 'M.query'
query :: (HasMysql m, QueryParams q, QueryResults r)
      => M.Query -> q -> m [r]
query q params = withMysql (\c -> M.query c q params)


------------------------------------------------------------------------------
-- | See 'M.query_'
query_ :: (HasMysql m, QueryResults r) => M.Query -> m [r]
query_ q = withMysql (\c -> M.query_ c q)

------------------------------------------------------------------------------
-- |
fold :: (HasMysql m,
         QueryResults row,
         QueryParams params,
         MonadCatchIO m)
     => M.Query -> params -> b -> (b -> row -> IO b) -> m b
fold template qs a f = withMysql (\c -> M.fold c template qs a f)


------------------------------------------------------------------------------
-- |
fold_ :: (HasMysql m,
          QueryResults row,
          MonadCatchIO m)
      => M.Query -> b -> (b -> row -> IO b) -> m b
fold_ template a f = withMysql (\c -> M.fold_ c template a f)


------------------------------------------------------------------------------
-- |
forEach :: (HasMysql m,
            QueryResults r,
            QueryParams q,
            MonadCatchIO m)
        => M.Query -> q -> (r -> IO ()) -> m ()
forEach template qs f = withMysql (\c -> M.forEach c template qs f)


------------------------------------------------------------------------------
-- |
forEach_ :: (HasMysql m,
             QueryResults r,
             MonadCatchIO m)
         => M.Query -> (r -> IO ()) -> m ()
forEach_ template f = withMysql (\c -> M.forEach_ c template f)


------------------------------------------------------------------------------
-- |
execute :: (HasMysql m, QueryParams q, MonadCatchIO m)
        => M.Query -> q -> m Int64
execute template qs = withMysql (\c -> M.execute c template qs)


------------------------------------------------------------------------------
-- |
execute_ :: (HasMysql m, MonadCatchIO m)
         => M.Query -> m Int64
execute_ template = withMysql (\c -> M.execute_ c template)


------------------------------------------------------------------------------
-- |
executeMany :: (HasMysql m, QueryParams q, MonadCatchIO m)
        => M.Query -> [q] -> m Int64
executeMany template qs = withMysql (\c -> M.executeMany c template qs)


rollback :: (HasMysql m, MonadCatchIO m) => m ()
rollback = withMysql M.rollback


commit :: (HasMysql m, MonadCatchIO m) => m ()
commit = withMysql M.commit


withTransaction :: (HasMysql m, MonadCatchIO m) => m a -> m a
withTransaction action = do
    r <- action `CIO.onException` rollback
    commit
    return r


formatMany :: (QueryParams q, HasMysql m, MonadCatchIO m)
           => M.Query -> [q] -> m ByteString
formatMany q qs = withMysql (\c -> M.formatMany c q qs)


formatQuery :: (QueryParams q, HasMysql m, MonadCatchIO m)
            => M.Query -> q -> m ByteString
formatQuery q qs = withMysql (\c -> M.formatQuery c q qs)
