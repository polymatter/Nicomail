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

postNewReminderR :: Handler RepHtml
postNewReminderR = return undefined

todayAsTriple :: Handler (Integer,Int,Int)
todayAsTriple = (liftIO getCurrentTime) >>= (return . toGregorian . utctDay) 

getSendEmailsR :: DoM -> MoY -> Handler RepHtml
getSendEmailsR day month = do
  reminderList <- runDB $ selectList [ReminderDay ==. day, ReminderMonth ==. month] []
  runInnerHandler <- handlerToIO
  _ <- liftIO $ forkIO $ runInnerHandler $ do
    _ <- sequence ( Import.map 
      (\r -> liftIO (mkReminderMail (reminderEmail $ entityVal r) (reminderContent $ entityVal r) >>= renderSendMail)) 
      reminderList )
    return ()
  setMessage "Emails Sent"
  redirect HomeR -- need to end with a Handler somehow but don't really want to do anything
  
getSendTodaysEmailR :: Handler RepHtml
getSendTodaysEmailR = do
  (_, monthAsInt, dayAsInt) <- todayAsTriple
  (month, day) <- return ( MoY monthAsInt , DoM dayAsInt)
  redirect $ SendEmailsR day month

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

mkReminderMail :: T.Text -> Html -> IO Mail
mkReminderMail emailaddr content = 
  mkMail 
    emailaddr
    "polymatter@112358.eu" 
    "Test Reminder Email"
    (renderHtml content)

mkMail :: T.Text -> T.Text -> T.Text -> LT.Text -> IO Mail
mkMail toaddr fromaddr title contents = 
  simpleMail
    (Address (Just toaddr)   toaddr)
    (Address (Just fromaddr) "The Memory Dopefish of Memories" )
    title
    contents
    contents
    []

-- insert :: val -> m (Key val)
-- update :: Key val -> [Update val] -> m ()
-- replace :: Key val -> val -> m ()
postUpdateReminderR :: ReminderId -> Handler RepHtml
postUpdateReminderR reminderId = do
  maybeLogin <- maybeAuth
  case maybeLogin of
    Nothing -> do
      setMessage "You can not edit a reminder while you are not logged in"
      redirect ReminderindexR
    Just userEntity -> do
      userId <- return $ entityKey userEntity
      email <- return $ userEmail $ entityVal userEntity
      mayberem <- runDB $ get reminderId
      case mayberem of
        Just reminder -> do
          day <- return $ reminderDay reminder
          month <- return $ reminderMonth reminder
          ((res, reminderForm),enctype) <- runFormPost (enterReminder day month "" email userId)
          case res of
            FormSuccess reminderR -> do
              runDB (update reminderId [ReminderContent =. (reminderContent reminderR)])
              setMessage "Successfully updated a reminder/journal entry"
              redirect $ ReminderR day month
            _ -> do
              setMessage "There was some sort of problem with the form. No idea what. Try again. See if it works this time" 
              redirect $ ReminderR day month
        Nothing -> do
          setMessage "We have no idea what happened. Somehow, this particular reminder was not found in the database"
          redirect $ ReminderindexR

reminderBox :: FieldSettings master
reminderBox = FieldSettings { 
  fsId = Just "reminderBox", 
  fsName = Just "reminderBox", 
  fsLabel = "What do you want to remember about this day?",
  fsTooltip = Nothing,
  fsAttrs = [("rows", "3")]
  }

-- renderDivs :: FormRender sub master a
-- areq :: Field sub master a -> FieldSettings master -> Maybe a -> AForm sub master a
enterReminder :: DoM -> MoY -> Html -> T.Text -> UserId -> Form Reminder
enterReminder day month content email userId = renderDivs $ Reminder
    <$> areq hiddenField "" (Just day)
    <*> areq hiddenField "" (Just month)
    <*> areq nicHtmlField reminderBox (Just content)
    <*> areq hiddenField "" (Just email)
    <*> areq hiddenField "" (Just userId)

getReminderindexR :: Handler RepHtml
getReminderindexR = do
  (_, todayMonth, todayDay) <- todayAsTriple
  (day, month) <- return (nonexistantDay, nonexistantMonth)
  yearview <- return $(widgetFile "yearview")
  defaultLayout $(widgetFile "yearview-wrapper")

-- defaultLayout :: Yesod a => GWidget sub a () -> GHandler sub a RepHtml
getReminderR :: DoM -> MoY -> Handler RepHtml
getReminderR day month = do
  maybeLogin <- maybeAuth
  case maybeLogin of
    Nothing -> do
      setMessage "You need to login to view the reminders on a specific day"
      redirect $ ReminderindexR
    Just userEntity -> do
      (_, todayMonth, todayDay) <- todayAsTriple
      userId <- return $ entityKey userEntity
      email <- return $ userEmail $ entityVal userEntity
      (newReminder, _) <- generateFormPost $ enterReminder day month "" email userId
      reminderList <- runDB $ selectList [ReminderDay ==. day, ReminderMonth ==. month, ReminderOwnerId ==. userId] [LimitTo 1]
      reminderRs <- Import.sequence $ Import.map 
                        (\r -> do
                            (widget, _) <- generateFormPost $ enterReminder day month (reminderContent $ entityVal r) email userId
                            reminderId <- return $ entityKey r
                            ident <- newIdent
                            synopsis <- (return . mksynopsis . renderHtml . reminderContent . entityVal) r
                            return $ ReminderView {
                              reminderId=reminderId, widget=widget, ident=ident, synopsis=synopsis
                              }
                        )
                        reminderList
      yearview <- return $(widgetFile "yearview")
      reminder <- return $(widgetFile "reminder")
      defaultLayout $ do $(widgetFile "reminder-wrapper")
        where mksynopsis :: LT.Text -> LT.Text
              mksynopsis = (flip LT.append) " ..." . LT.concat . Import.map (LT.append " ") . Import.take 6 . LT.words

data ReminderView = ReminderView { reminderId :: ReminderId 
                                 , widget :: Widget
                                 , ident :: T.Text
                                 , synopsis :: LT.Text
                                 }
                 
-- represents a brand new reminder when none has been created before
blankReminder :: DoM -> MoY -> T.Text -> UserId -> Reminder
blankReminder day month email userId = Reminder day month "blank" email userId

-- represents the null Day. needed because we can't have undefined parameters and yearview currently needs a day
nonexistantDay :: DoM
nonexistantDay = DoM 0

-- represents the null Month. needed because we can't have undefined parameters and yearview currently needs a 
-- month
nonexistantMonth :: MoY
nonexistantMonth = MoY 0

-- this is not the Show instance because the Show typeclass defines other classes I don't need right now
-- otherwise, yes this could be a Show instance
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
               
getMonthNameShort :: MoY -> T.Text
getMonthNameShort = T.toUpper . (T.take 3) . getMonthName

-- | Bypass function: (..) function inaccessible. This replicates the behaviour for hamlet template
listFrom1ToX :: Int -> [Int]
listFrom1ToX x = G.reverse $ L.unfoldr (\i -> if i <= 0 then Nothing else Just (i, i-1)) x

-- | Bypass function: (&&) function inaccessible. This replicates the behaviour for hamlet template
boolAnd :: Bool -> Bool -> Bool
boolAnd True b = b
boolAnd _    _ = False

-- | selects the appropriate class for each day
cssClass :: (DoM, MoY) -> (DoM, MoY) -> (DoM, MoY) -> T.Text
cssClass selected today current@(DoM d, MoY m)
  | current == selected    = "selectedday"
  | current == today       = "todayday" 
  | d > monthLength True m = "emptyday"
  | otherwise              = "day"
                          
monthSpan :: Int
monthSpan = 16

monthColumns :: Int
monthColumns = 1

monthHeadingOnTop :: Bool
monthHeadingOnTop = False

dayFromRowAndColumn :: Int -> Int -> Int
dayFromRowAndColumn row col = (row-1) * monthSpan + col