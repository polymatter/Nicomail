{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Reminder where

import Import
import Yesod.Form.Nic (YesodNic, nicHtmlField)
--import Yesod.Form.Jquery --may be interesting date stuff in here for later
--import Text.Email.Validate --to validate email addresses
import Network.Mail.Mime
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Control.Concurrent
import Data.Time
import Data.Time.Calendar.MonthDay (monthLength)
import Data.Text as T
import Data.Text.Lazy as LT
import Yesod.Auth (maybeAuth)
import Data.List as L (unfoldr)
import GHC.List as G (reverse)

instance YesodNic App

-- liftIO :: IO a -> m a
--getSendemailR :: ReminderId -> Handler RepHtml
--getSendemailR reminderId = do
--  reminder <- runDB (get404 reminderId)
--  runInnerHandler <- handlerToIO
--  _ <- liftIO $ forkIO $ runInnerHandler $ do
--    liftIO $ testmail (reminderContent reminder) >>= renderSendMail
--  redirect $ ReminderindexR

getSendTodaysEmailR :: Handler RepHtml
getSendTodaysEmailR = do
  (_, monthAsInt, dayAsInt) <- (liftIO getCurrentTime) >>= (return . toGregorian . utctDay) 
  (month, day) <- return ( MoY monthAsInt , DoM dayAsInt)
  maybeReminder <- runDB $ selectFirst 
                   [ReminderDay ==. day, ReminderMonth ==. month] 
                   [LimitTo 1]
  runInnerHandler <- handlerToIO  
  case (maybeReminder :: Maybe (Entity Reminder)) of
    Nothing -> do 
      _ <- liftIO $ forkIO $ runInnerHandler $
        liftIO $ (mailNotFound day month) >>= renderSendMail
      redirect ( HomeR )
    Just reminderEntity -> do
      _ <- liftIO $ forkIO $ runInnerHandler $ do
        liftIO $ (mailReminder . reminderContent . entityVal) reminderEntity >>= renderSendMail
      redirect ( HomeR )

mailNotFound :: DoM -> MoY -> IO Mail
mailNotFound day month =
  mkMail
    "nicomail@mailinator.com"
    "polymatter@112358.eu"
    "No Reminder Email Today"
    (renderHtml . toHtml . LT.concat $
     ["No Reminder email found for ", 
      LT.pack . show $ day,
      "/",
      LT.pack . show $ month] )

mailReminder :: Html -> IO Mail
mailReminder content = 
  mkMail 
    "nicomail@mailinator.com" 
    "polymatter@112358.eu" 
    "Test Reminder Email"
    (renderHtml content)

mkMail :: T.Text -> T.Text -> T.Text -> LT.Text -> IO Mail
mkMail toaddr fromaddr title contents = 
  simpleMail
    (Address (Just toaddr)   toaddr)
    (Address (Just "The Memory Dopefish of Memories") fromaddr )
    title
    contents
    contents
    []

-- insert :: val -> m (Key val)
-- update :: Key val -> [Update val] -> m ()
-- replace :: Key val -> val -> m ()
postReminderR :: DoM -> MoY -> Handler RepHtml
postReminderR day month = do
  maybeLogin <- maybeAuth
  case maybeLogin of
    Nothing -> defaultLayout $ do
      setTitle "You are not logged in"
      $(widgetFile "reminderError")
    Just userEntity -> do
      userId <- return $ entityKey userEntity
      ((res, reminderForm),enctype) <- runFormPost (enterReminder day month "" userId)
      case res of
        FormSuccess reminderR -> do
          mayberem <- runDB $ getBy $ UniqueReminder day month userId
          case mayberem of
            Just reminderEntity -> do
              runDB (update (entityKey reminderEntity) [ReminderContent =. (reminderContent reminderR)])
              redirect $ ReminderR day month
            Nothing -> defaultLayout $ do
              setTitle "Can not find that date" 
              $(widgetFile "reminderError")
        _ -> defaultLayout $ do
          setTitle "Please correct your entry form"
          $(widgetFile "reminderAddError")

reminderBox :: FieldSettings master
reminderBox = FieldSettings { 
  fsId = Just "reminderBox", 
  fsName = Just "reminderBox", 
  fsLabel = "What do you want to remember about this day?",
  fsTooltip = Nothing,
  fsAttrs = [("class", "reminderBox")]
  }

-- renderDivs :: FormRender sub master a
-- areq :: Field sub master a -> FieldSettings master -> Maybe a -> AForm sub master a
enterReminder :: DoM -> MoY -> Html -> UserId -> Form Reminder
enterReminder day month content userId = renderDivs $ Reminder
    <$> areq hiddenField "" (Just day)
    <*> areq hiddenField "" (Just month)
    <*> areq nicHtmlField reminderBox (Just content)
    <*> areq hiddenField "" (Just userId)

getReminderindexR :: Handler RepHtml
getReminderindexR = do
  yearview <- return $(widgetFile "yearview")
  defaultLayout $(widgetFile "yearview-wrapper")

-- defaultLayout :: Yesod a => GWidget sub a () -> GHandler sub a RepHtml
getReminderR :: DoM -> MoY -> Handler RepHtml
getReminderR day month = do
  maybeLogin <- maybeAuth
  case maybeLogin of
    Nothing -> redirect HomeR
    Just userEntity -> do
      userId <- return $ entityKey userEntity
      mayberem <- runDB $ getBy $ UniqueReminder day month userId
      case mayberem of
        Nothing -> do
          _ <- runDB $ insert $ blankReminder day month userId
          reminderR <- return $ blankReminder day month userId
          (reminderForm, enctype) <- generateFormPost (enterReminder day month "" userId)
          yearview <- return $(widgetFile "yearview")
          reminder <- return $(widgetFile "reminder")
          defaultLayout $ do $(widgetFile "reminder-wrapper")     
        Just reminderEntity -> do
          reminderR <- return $ entityVal reminderEntity
          (reminderForm, enctype) <- generateFormPost (enterReminder day month (reminderContent reminderR) userId)
          yearview <- return $(widgetFile "yearview")
          reminder <- return $(widgetFile "reminder")
          defaultLayout $ do $(widgetFile "reminder-wrapper")      
                         
blankReminder :: DoM -> MoY -> UserId -> Reminder
blankReminder day month userId = Reminder day month "blank" userId


getMonthName :: MoY -> T.Text
getMonthName (MoY i)
 | i == 1    = "January"
 | i == 2    = "February"
 | i == 3    = "March"
 | i == 4    = "April"
 | i == 5    = "May"
 | i == 6    = "June"
 | i == 7    = "July"
 | i == 8    = "August"
 | i == 9    = "September"
 | i == 10   = "October"
 | i == 11   = "November"
 | i == 12   = "December"
 | otherwise = T.concat ["invalid month id: ", T.pack $ show i]
               
listFrom1ToX :: Int -> [Int]
listFrom1ToX x = G.reverse $ L.unfoldr (\i -> if i <= 0 then Nothing else Just (i, i-1)) x