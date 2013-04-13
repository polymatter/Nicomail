{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Reminder where

import Import

-- defaultLayout :: Yesod a => GWidget sub a () -> GHandler sub a RepHtml
getReminderR :: Handler RepHtml
getReminderR = do
  defaultLayout $ do $(widgetFile "yearview")