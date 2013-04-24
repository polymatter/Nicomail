{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Reminder where

import Import
import Yesod.Form.Nic (YesodNic, nicHtmlField)
--import Yesod.Form.Jquery --may be interesting date stuff in here for later
import Network.Mail.Mime
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Control.Concurrent
import Data.Time

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
  (nowDay+1) <- (liftIO getCurrentTime) >>= (return . thd . toGregorian . utctDay) 
  maybeReminder <- runDB $ selectFirst [ReminderContent !=. "secret day"] [OffsetBy nowDay, LimitTo 1]
  runInnerHandler <- handlerToIO  
  case (maybeReminder :: Maybe (Entity Reminder)) of
    Nothing -> do 
      _ <- liftIO $ forkIO $ runInnerHandler $ do
        liftIO $ testmail "Nothing here mate" >>= renderSendMail
      redirect ( ReminderindexR )
    Just reminderEntity -> do
      _ <- liftIO $ forkIO $ runInnerHandler $ do
        liftIO $ (testmail . reminderContent . entityVal) reminderEntity >>= renderSendMail
      redirect ( ReminderindexR )
  where thd = \(_, _, c) -> c

testmail :: Html -> IO Mail
testmail content = 
  mymail 
    "nicomail@mailinator.com" 
    "polymatter@112358.eu" 
    "Test Reminder Email"
    (renderHtml content)

--mymail :: Text -> Text -> Text -> Data.Text.Lazy.Internal.Text -> IO Mail
mymail toaddr fromaddr title contents = 
  simpleMail
    (Address (Just toaddr)   toaddr)
    (Address (Just "The Memory Dopefish of Memories") "dopefish@gov.com" )
    title
    contents
    contents
    []

-- insert :: val -> m (Key val)
-- update :: Key val -> [Update val] -> m ()
-- replace :: Key val -> val -> m ()
postReminderR :: Int -> Int -> Handler RepHtml
postReminderR day month = do
  ((res, reminderForm),enctype) <- runFormPost (enterReminder day month "")
  case res of
    FormSuccess reminderR -> do
      mayberem <- runDB $ selectFirst [ReminderDay ==. day, ReminderMonth ==. month] []
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
enterReminder :: Int -> Int -> Html -> Form Reminder
enterReminder day month content = renderDivs $ Reminder
    <$> areq hiddenField "" (Just day)
    <*> areq hiddenField "" (Just month)
    <*> areq nicHtmlField reminderBox (Just content)

getReminderindexR :: Handler RepHtml
getReminderindexR = do 
  yearview <- return $(widgetFile "yearview")
  defaultLayout $(widgetFile "yearview-wrapper")

-- defaultLayout :: Yesod a => GWidget sub a () -> GHandler sub a RepHtml
getReminderR :: Int -> Int -> Handler RepHtml
getReminderR day month = do
  mayberem <- runDB $ selectFirst [ReminderDay ==. day, ReminderMonth ==. month] []
  case mayberem of
    Nothing -> do
      _ <- runDB $ insert $ blankReminder day month
      reminderR <- return $ blankReminder day month
      (reminderForm, enctype) <- generateFormPost (enterReminder day month "")
      yearview <- return $(widgetFile "yearview")
      reminder <- return $(widgetFile "reminder")
      defaultLayout $ do $(widgetFile "reminder-wrapper")     
    Just reminderEntity -> do
      reminderR <- return $ entityVal reminderEntity
      (reminderForm, enctype) <- generateFormPost (enterReminder day month (reminderContent reminderR))
      yearview <- return $(widgetFile "yearview")
      reminder <- return $(widgetFile "reminder")
      defaultLayout $ do $(widgetFile "reminder-wrapper")      
                         
blankReminder :: Int -> Int -> Reminder
blankReminder day month = Reminder day month "blank"

-- runDB :: (YesodPersist master) 
--    => YesodDB sub master a -> GHandler sub master a

-- insert 

-- seedDatabase