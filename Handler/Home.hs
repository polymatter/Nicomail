{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth


getHomeR :: Handler RepHtml
getHomeR = do
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Dopefish Reminder Journal of Memories"
        $(widgetFile "homepage")
