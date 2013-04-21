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
getSendemailR :: ReminderId -> Handler RepHtml
getSendemailR reminderId = do
  reminder <- runDB (get404 reminderId)
  runInnerHandler <- handlerToIO
  _ <- liftIO $ forkIO $ runInnerHandler $ do
    liftIO $ testmail (reminderContent reminder) >>= renderSendMail
  redirect $ ReminderR reminderId

getSendTodaysEmailR :: Handler RepHtml
getSendTodaysEmailR = do
  nowDay <- (liftIO getCurrentTime) >>= (return . thd . toGregorian . utctDay) 
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
    "glisher_rock@hotmail.com" 
    "polymatter@112358.eu" 
    "Test Reminder Email"
    (renderHtml content)

--mymail :: Text -> Text -> Text -> Data.Text.Lazy.Internal.Text -> IO Mail
mymail toaddr fromaddr title contents = 
  simpleMail
    (Address (Just toaddr)   toaddr)
    (Address (Just fromaddr) fromaddr )
    title
    contents
    contents
    []

-- insert :: val -> m (Key val)
-- update :: Key val -> [Update val] -> m ()
-- replace :: Key val -> val -> m ()
postReminderR :: ReminderId -> Handler RepHtml
postReminderR reminderId = do
  ((res, reminderForm),enctype) <- runFormPost (enterReminder "")
  case res of
    FormSuccess reminderR ->
      runDB (update reminderId [ReminderContent =. (reminderContent reminderR)])
      >>
      (redirect $ ReminderR reminderId)
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
enterReminder :: Html -> Form Reminder
enterReminder previousContent = renderDivs $ Reminder
    <$> areq nicHtmlField reminderBox (Just previousContent)

getReminderindexR :: Handler RepHtml
getReminderindexR = do 
  yearview <- return $(widgetFile "yearview")
  defaultLayout $(widgetFile "yearview-wrapper")

-- defaultLayout :: Yesod a => GWidget sub a () -> GHandler sub a RepHtml
getReminderR :: ReminderId -> Handler RepHtml
getReminderR reminderId = do
  mayberem <- runDB $ get reminderId
  case mayberem of
    Nothing -> do
      reminderId <- runDB $ insert blankReminder
      reminderR <- return blankReminder
      (reminderForm, enctype) <- generateFormPost (enterReminder (reminderContent reminderR))
      yearview <- return $(widgetFile "yearview")
      reminder <- return $(widgetFile "reminder")
      defaultLayout $ do $(widgetFile "reminder-wrapper")
    Just reminderR -> do
      (reminderForm, enctype) <- generateFormPost (enterReminder (reminderContent reminderR))
      yearview <- return $(widgetFile "yearview")
      reminder <- return $(widgetFile "reminder")
      defaultLayout $ do $(widgetFile "reminder-wrapper")      
                         
blankReminder :: Reminder
blankReminder = Reminder "blank"