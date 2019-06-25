{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|

This module allows you to use the auth snaplet with your user database stored
in a MySQL database.  When you run your application with this snaplet, a
config file will be copied into the the @snaplets/mysql-auth@ directory.
This file contains all of the configurable options for the snaplet and allows
you to change them without recompiling your application.

To use this snaplet in your application enable the session, mysql, and auth
snaplets as follows:

> data App = App
>     { ... -- your own application state here
>     , _sess :: Snaplet SessionManager
>     , _db   :: Snaplet Mysql
>     , _auth :: Snaplet (AuthManager App)
>     }

Then in your initializer you'll have something like this:

> d <- nestSnaplet "db" db mysqlInit
> a <- nestSnaplet "auth" auth $ initMysqlAuth sess d

If you have not already created the database table for users, it will
automatically be created for you the first time you run your application.

-}

module Snap.Snaplet.Auth.Backends.MysqlSimple
  ( initMysqlAuth
  ) where

------------------------------------------------------------------------------
import           Prelude
import           Control.Lens
import           Control.Error
import qualified Control.Exception as E
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Configurator as C
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Pool
import qualified Database.MySQL.Simple as M
import           Database.MySQL.Simple.Param
import           Database.MySQL.Simple.Result
import           Database.MySQL.Simple.Types
import           Database.MySQL.Simple.QueryResults
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.MysqlSimple
import           Snap.Snaplet.Session
import           Web.ClientSession
import           Paths_snaplet_mysql_simple


data MysqlAuthManager = MysqlAuthManager
    { pamTable    :: AuthTable
    , pamConnPool :: Pool M.Connection
    }


------------------------------------------------------------------------------
-- | Initializer for the mysql backend to the auth snaplet.
--
initMysqlAuth
  :: SnapletLens b SessionManager  -- ^ Lens to the session snaplet
  -> Snaplet Mysql  -- ^ The mysql snaplet
  -> SnapletInit b (AuthManager b)
initMysqlAuth sess db = makeSnaplet "mysql-auth" desc datadir $ do
    config <- getSnapletUserConfig
    authTable <- liftIO $ C.lookupDefault "snap_auth_user" config "authTable"
    authSettings <- authSettingsFromConfig
    key <- liftIO $ getKey (asSiteKey authSettings)
    let tableDesc = defAuthTable { tblName = authTable }
    let manager = MysqlAuthManager tableDesc $ mysqlPool $ db ^# snapletValue
    liftIO $ createTableIfMissing manager
    rng <- liftIO mkRNG
    return $ AuthManager
      { backend = manager
      , session = sess
      , activeUser = Nothing
      , minPasswdLen = asMinPasswdLen authSettings
      , rememberCookieName = asRememberCookieName authSettings
      , rememberCookieDomain = Nothing
      , rememberPeriod = asRememberPeriod authSettings
      , siteKey = key
      , lockout = asLockout authSettings
      , randomNumberGenerator = rng
      }
  where
    desc = "A MySQL backend for user authentication"
    datadir = Just $ liftM (++"/resources/auth") getDataDir


------------------------------------------------------------------------------
-- | Create the user table if it doesn't exist.
createTableIfMissing :: MysqlAuthManager -> IO ()
createTableIfMissing MysqlAuthManager{..} = do
    withResource pamConnPool $ \conn -> do
        M.execute_ conn (Query $ T.encodeUtf8 q)
    return ()
  where
    q = T.concat
          [ "CREATE TABLE IF NOT EXISTS "
          , tblName pamTable
          , "("
          , T.intercalate ", " (map (fDesc . ($pamTable) . (fst)) colDef)
          , ")"
          ]


buildUid :: Int -> UserId
buildUid = UserId . T.pack . show


instance Result UserId where
    convert f v = buildUid $ convert f v

instance Result Password where
    convert f v = Encrypted $ convert f v

instance QueryResults AuthUser where
    convertResults [f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18]
                   [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18] =
        AuthUser
            _userId
            _userLogin
            _userEmail
            _userPassword
            _userActivatedAt
            _userSuspendedAt
            _userRememberToken
            _userLoginCount
            _userFailedLoginCount
            _userLockedOutUntil
            _userCurrentLoginAt
            _userLastLoginAt
            _userCurrentLoginIp
            _userLastLoginIp
            _userCreatedAt
            _userUpdatedAt
            _userResetToken
            _userResetRequestedAt
            _userRoles
            _userMeta
      where
        !_userId               = convert f1 b1
        !_userLogin            = convert f2 b2
        !_userEmail            = convert f3 b3
        !_userPassword         = convert f4 b4
        !_userActivatedAt      = convert f5 b5
        !_userSuspendedAt      = convert f6 b6
        !_userRememberToken    = convert f7 b7
        !_userLoginCount       = convert f8 b8
        !_userFailedLoginCount = convert f9 b9
        !_userLockedOutUntil   = convert f10 b10
        !_userCurrentLoginAt   = convert f11 b11
        !_userLastLoginAt      = convert f12 b12
        !_userCurrentLoginIp   = convert f13 b13
        !_userLastLoginIp      = convert f14 b14
        !_userCreatedAt        = convert f15 b15
        !_userUpdatedAt        = convert f16 b16
        !_userResetToken       = convert f17 b17
        !_userResetRequestedAt = convert f18 b18
        !_userRoles            = []
        !_userMeta             = HM.empty
    convertResults fs vs = do
      convertError fs vs 18


querySingle :: (QueryParams q, QueryResults a)
            => Pool M.Connection -> Query -> q -> IO (Maybe a)
querySingle pool q ps = withResource pool $ \conn -> return . listToMaybe =<<
    M.query conn q ps

authExecute :: QueryParams q
            => Pool M.Connection -> Query -> q -> IO ()
authExecute pool q ps = do
    _ <- withResource pool $ \conn -> M.execute conn q ps
    return ()

instance Param Password where
    render (ClearText bs) = render bs
    render (Encrypted bs) = render bs


-- | Datatype containing the names of the columns for the authentication table.
data AuthTable
  =  AuthTable
  {  tblName             :: Text
  ,  colId               :: (Text, Text)
  ,  colLogin            :: (Text, Text)
  ,  colEmail            :: (Text, Text)
  ,  colPassword         :: (Text, Text)
  ,  colActivatedAt      :: (Text, Text)
  ,  colSuspendedAt      :: (Text, Text)
  ,  colRememberToken    :: (Text, Text)
  ,  colLoginCount       :: (Text, Text)
  ,  colFailedLoginCount :: (Text, Text)
  ,  colLockedOutUntil   :: (Text, Text)
  ,  colCurrentLoginAt   :: (Text, Text)
  ,  colLastLoginAt      :: (Text, Text)
  ,  colCurrentLoginIp   :: (Text, Text)
  ,  colLastLoginIp      :: (Text, Text)
  ,  colCreatedAt        :: (Text, Text)
  ,  colUpdatedAt        :: (Text, Text)
  ,  colResetToken       :: (Text, Text)
  ,  colResetRequestedAt :: (Text, Text)
  ,  rolesTable          :: Text
  }

-- | Default authentication table layout
defAuthTable :: AuthTable
defAuthTable
  =  AuthTable
  {  tblName             = "snap_auth_user"
  ,  colId               = ("uid", "SERIAL PRIMARY KEY")
  ,  colLogin            = ("login", "VARCHAR(64) UNIQUE NOT NULL")
  ,  colEmail            = ("email", "VARCHAR(255)")
  ,  colPassword         = ("password", "VARCHAR(255)")
  ,  colActivatedAt      = ("activated_at", "TIMESTAMP")
  ,  colSuspendedAt      = ("suspended_at", "TIMESTAMP")
  ,  colRememberToken    = ("remember_token", "VARCHAR(255)")
  ,  colLoginCount       = ("login_count", "INTEGER NOT NULL")
  ,  colFailedLoginCount = ("failed_login_count", "INTEGER NOT NULL")
  ,  colLockedOutUntil   = ("locked_out_until", "TIMESTAMP NULL DEFAULT NULL")
  ,  colCurrentLoginAt   = ("current_login_at", "TIMESTAMP")
  ,  colLastLoginAt      = ("last_login_at", "TIMESTAMP")
  ,  colCurrentLoginIp   = ("current_login_ip", "VARCHAR(255)")
  ,  colLastLoginIp      = ("last_login_ip", "VARCHAR(255)")
  ,  colCreatedAt        = ("created_at", "TIMESTAMP")
  ,  colUpdatedAt        = ("updated_at", "TIMESTAMP")
  ,  colResetToken       = ("reset_token", "VARCHAR(255)")
  ,  colResetRequestedAt = ("reset_requested_at", "TIMESTAMP")
  ,  rolesTable          = "user_roles"
  }

fDesc :: (Text, Text) -> Text
fDesc f = fst f `T.append` " " `T.append` snd f

-- | List of deconstructors so it's easier to extract column names from an
-- 'AuthTable'.
colDef :: [(AuthTable -> (Text, Text), AuthUser -> Action)]
colDef =
  [ (colId              , render . fmap unUid . userId)
  , (colLogin           , render . userLogin)
  , (colEmail           , render . userEmail)
  , (colPassword        , render . userPassword)
  , (colActivatedAt     , render . userActivatedAt)
  , (colSuspendedAt     , render . userSuspendedAt)
  , (colRememberToken   , render . userRememberToken)
  , (colLoginCount      , render . userLoginCount)
  , (colFailedLoginCount, render . userFailedLoginCount)
  , (colLockedOutUntil  , render . userLockedOutUntil)
  , (colCurrentLoginAt  , render . userCurrentLoginAt)
  , (colLastLoginAt     , render . userLastLoginAt)
  , (colCurrentLoginIp  , render . userCurrentLoginIp)
  , (colLastLoginIp     , render . userLastLoginIp)
  , (colCreatedAt       , render . userCreatedAt)
  , (colUpdatedAt       , render . userUpdatedAt)
  , (colResetToken      , render . userResetToken)
  , (colResetRequestedAt, render . userResetRequestedAt)
  ]

colNames :: AuthTable -> T.Text
colNames pam =
  T.intercalate "," . map (\(f,_) -> fst (f pam)) $ colDef

saveQuery :: AuthTable -> AuthUser -> (Text, [Action])
saveQuery at u@AuthUser{..} = maybe insertQuery updateQuery userId
  where
    insertQuery =  (T.concat [ "INSERT INTO "
                             , tblName at
                             , " ("
                             , T.intercalate "," cols
                             , ") VALUES ("
                             , T.intercalate "," vals
                             , ");"
                             ]
                   , params)
    qval f  = fst (f at) `T.append` " = ?"
    updateQuery uid =
        (T.concat [ "UPDATE "
                  , tblName at
                  , " SET "
                  , T.intercalate "," (map (qval . fst) $ tail colDef)
                  , " WHERE "
                  , fst (colId at)
                  , " = ? ;"
                  ]
        , params ++ [render $ unUid uid])
    cols = map (fst . ($at) . fst) $ tail colDef
    vals = map (const "?") cols
    params = map (($u) . snd) $ tail colDef


onFailure :: Monad m => E.SomeException -> m (Either AuthFailure a)
onFailure e = return $ Left $ AuthError $ show e

------------------------------------------------------------------------------
-- |
instance IAuthBackend MysqlAuthManager where
    save MysqlAuthManager{..} u@AuthUser{..} = do
        let (qstr, params) = saveQuery pamTable u
        let q = Query $ T.encodeUtf8 qstr
        let action = withResource pamConnPool $ \conn -> do
                res <- M.execute conn q params
                return $ Right u
        E.catch action onFailure

    lookupByUserId MysqlAuthManager{..} uid = do
        let q = Query $ T.encodeUtf8 $ T.concat
                [ "select ", colNames pamTable, " from "
                , tblName pamTable
                , " where "
                , fst (colId pamTable)
                , " = ?;"
                ]
        querySingle pamConnPool q [unUid uid]

    lookupByLogin MysqlAuthManager{..} login = do
        let q = Query $ T.encodeUtf8 $ T.concat
                [ "select ", colNames pamTable, " from "
                , tblName pamTable
                , " where "
                , fst (colLogin pamTable)
                , " = ?;"
                ]
        querySingle pamConnPool q [login]

    lookupByRememberToken MysqlAuthManager{..} token = do
        let q = Query $ T.encodeUtf8 $ T.concat
                [ "select ", colNames pamTable, " from "
                , tblName pamTable
                , " where "
                , fst (colRememberToken pamTable)
                , " = ?;"
                ]
        querySingle pamConnPool q [token]

    destroy MysqlAuthManager{..} AuthUser{..} = do
        let q = Query $ T.encodeUtf8 $  T.concat
                [ "delete from "
                , tblName pamTable
                , " where "
                , fst (colLogin pamTable)
                , " = ?;"
                ]
        authExecute pamConnPool q [userLogin]
