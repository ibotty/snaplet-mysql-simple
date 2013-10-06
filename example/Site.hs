{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Control.Monad.State
import           Data.ByteString (ByteString)
import           Control.Lens
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock
import qualified Database.MySQL.Simple as S
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.MysqlSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.MysqlSimple
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import           Text.XmlHtml hiding (render)


------------------------------------------------------------------------------
data App = App
    { _sess :: Snaplet SessionManager
    , _db :: Snaplet Mysql
    , _auth :: Snaplet (AuthManager App)
    }

makeLenses ''App

instance HasMysql (Handler b App) where
    getMysqlState = with db get

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/",            writeText "hello")
         , ("foo", fooHandler)
         , ("add/:uname", addHandler)
         ]

fooHandler = do
    results <- query_ "select * from snap_auth_user"
    liftIO $ print (results :: [AuthUser])

addHandler = do
    mname <- getParam "uname"
    let name = maybe "guest" T.decodeUtf8 mname
    u <- with auth $ createUser name ""
    liftIO $ print u

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    s <- nestSnaplet "" sess $
         initCookieSessionManager "site_key.txt" "_cookie" Nothing
    d <- nestSnaplet "db" db mysqlInit
    a <- nestSnaplet "auth" auth $ initMysqlAuth sess d
    addRoutes routes
    return $ App s d a


main :: IO ()
main = serveSnaplet defaultConfig app

