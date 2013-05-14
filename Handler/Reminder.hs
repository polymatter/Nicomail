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

todayAsTriple :: Handler (Integer,Int,Int)
todayAsTriple = (liftIO getCurrentTime) >>= (return . toGregorian . utctDay) 

getSendTodaysEmailR :: Handler RepHtml
getSendTodaysEmailR = do
  (_, monthAsInt, dayAsInt) <- todayAsTriple
  (month, day) <- return ( MoY monthAsInt , DoM dayAsInt)
  reminderList <- runDB $ selectList [ReminderDay ==. day, ReminderMonth ==. month] []
  sequence $ Import.map
    (\reminder -> do
        runInnerHandler <- handlerToIO  
        _ <- liftIO $ forkIO $ runInnerHandler $ 
          liftIO $ (mailReminder . reminderContent . entityVal) reminder >>= renderSendMail
        return ()
    ) 
    reminderList
  redirect HomeR -- need to end with a Handler somehow but don't really want to do anything

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
  (_, todayMonth, todayDay) <- todayAsTriple
  (day, month) <- return (nonexistantDay, nonexistantMonth)
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
      (_, todayMonth, todayDay) <- todayAsTriple
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
                         
-- represents a brand new reminder when none has been created before
blankReminder :: DoM -> MoY -> UserId -> Reminder
blankReminder day month userId = Reminder day month "blank" userId

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
monthSpan = 31

monthColumns :: Int
monthColumns = 1

monthHeadingOnTop :: Bool
monthHeadingOnTop = False

dayFromRowAndColumn :: Int -> Int -> Int
dayFromRowAndColumn row col = (row-1) * monthSpan + col