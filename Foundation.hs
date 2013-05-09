module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.Email
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist.Store
import Settings.StaticFiles
import Database.Persist.GenericSql
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile, shamlet)
import System.Log.FastLogger (Logger)
import Network.Mail.Mime
import Text.Shakespeare.Text (stext)
import qualified Data.Text.Lazy.Encoding
import Control.Monad (join)
import Data.Maybe (isJust)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    , appLogger :: Logger
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm App App (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
--  approot :: Yesod a => Approot a
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
--  makeSessionBackend :: Yesod a => a -> IO (Maybe (SessionBackend a))
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        let timeout = 120 * 60 -- 120 minutes
        (getCachedDate, _closeDateCache) <- clientSessionDateCacher timeout
        return . Just $ clientSessionBackend2 key getCachedDate

--  defaultLayout :: Yesod a => GWidget sub a () -> GHandler sub a RepHtml
    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        maybeLogin <- maybeAuth

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            --addStylesheet $ StaticR css_bootstrap_css --will add bootstrap links in default-layout-wrapper directly
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
--  urlRenderOverride :: Yesod a => a -> Route a -> Maybe Builder
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
--  authRoute :: Yesod a => a -> Maybe (Route a)
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
--  addStaticContent :: Yesod a => Text -> Text -> ByteString -> GHandler sub a (Maybe (Either Text (Route a, [(Text, Text)])))
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
--  jsLoader :: Yesod a => a -> ScriptLoadPosition a
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
--  shouldLog :: Yesod a => a -> LogSource -> LogLevel -> Bool
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

--  getLogger :: Yesod a => a -> IO Logger
    getLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
--  runDB :: YesodPersist master => YesodDB sub master a -> GHandler sub master a
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
--  loginDest :: YesodAuth master => master -> Route master
    loginDest _ = ReminderindexR
    
    -- Where to send a user after logout
--  logoutDest :: YesodAuth master => master -> Route master
    logoutDest _ = ReminderindexR

--  getAuthId :: YesodAuth master => Creds master -> GHandler sub master (Maybe (AuthId master))
--    getAuthId creds = runDB $ do
--        x <- getBy $ UniqueUser $ credsIdent creds
--        case x of
--            Just (Entity uid _) -> return $ Just uid
--            Nothing -> do
--                fmap Just $ insert $ User (credsIdent creds) Nothing

    getAuthId creds = runDB $ do
        x <- insertBy $ User (credsIdent creds) Nothing Nothing False
        return $ Just $
          case x of
            Left (Entity userid _) -> userid -- newly added user
            Right userid           -> userid -- existing user

    -- You can add other plugins like BrowserID, email or OAuth here
--  authPlugins :: YesodAuth master => master -> [AuthPlugin master]
--  authPlugins _ = [authBrowserId, authGoogleEmail]
    authPlugins _ = [authEmail]

--  authHttpManager :: YesodAuth master => master -> Manager
    authHttpManager = httpManager

-- email auth specific code
instance YesodAuthEmail App where
  type AuthEmailId App = UserId
  
  addUnverified email verkey = 
    runDB $ insert $ User email Nothing (Just verkey) False
    
  getVerifyKey = runDB . fmap (join . fmap userVerkey) . get
  
  setVerifyKey uid key = runDB $ update uid [UserVerkey =. Just key]

  verifyAccount uid = runDB $ do
    mu <- get uid
    case mu of
      Nothing -> return Nothing
      Just _ -> do
        update uid [UserVerified =. True]
        return $ Just uid
        
  getPassword = runDB . fmap (join . fmap userPassword) . get
  
  setPassword uid pass = runDB $ update uid [UserPassword =. Just pass]

  getEmailCreds email = runDB $ do
    mu <- getBy $ UniqueUser email
    case mu of
      Nothing -> return Nothing
      Just (Entity uid u) -> return $ Just EmailCreds
                             { emailCredsId = uid
                             , emailCredsAuthId = Just uid
                             , emailCredsStatus = isJust $ userPassword u
                             , emailCredsVerkey = userVerkey u
                             }
                             
  getEmail = runDB . fmap (fmap userEmail) . get

  sendVerifyEmail email _ verurl =
    liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply") 
       { mailTo = [Address Nothing email]
       , mailHeaders = [("Subject", "Verify your email address")]
       , mailParts = [[textPart, htmlPart]]
       }
    where
      textPart = Part { partType = "text/plain; charset=utf-8"
                      , partEncoding = None 
                      , partFilename = Nothing
                      , partHeaders = []
                      , partContent = Data.Text.Lazy.Encoding.encodeUtf8 [stext| 
Please confirm your email address by clicking on the link below
                                               
\#{verurl}

Thank You
|]
                      }
      htmlPart = Part { partType = "text/html; charset=utf-8"
                      , partEncoding = None
                      , partFilename  = Nothing
                      , partHeaders = []
                      , partContent = renderHtml [shamlet|
<p>Please configm your email address by clicking on the link below.
<p>
  <a href=#{verurl}>#{verurl}
<p>Thank you
|]
                      }


-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
--  renderMessage :: RenderMessage master message => master -> [Lang] -> master -> Text
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod
