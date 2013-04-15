{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Reminder where

import Import
import Yesod.Form.Nic (YesodNic, nicHtmlField)
instance YesodNic App

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

enterReminder :: Html -> Form Reminder
enterReminder previousContent = renderDivs $ Reminder
  <$> areq nicHtmlField "What do you want to remember about this day?" (Just previousContent)

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