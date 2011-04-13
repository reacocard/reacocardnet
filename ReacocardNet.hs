{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module ReacocardNet where

import Data.Time
import Data.Time.Format

import Yesod hiding (defaultLayout)
import Database.Persist.GenericSql
import Database.Persist.TH(share)

import Settings

data ReacocardNet = ReacocardNet 

mkYesod "ReacocardNet" [parseRoutes|
/ HomeR GET
/about AboutR GET
/code CodeR GET

/blog BlogR GET
|]

type ReacocardNetWidget = GGWidget ReacocardNet ReacocardNet (GHandler ReacocardNet ReacocardNet) ()

instance Yesod ReacocardNet where
    approot _ = ""

defaultLayout contents = do
    mCurrentRoute <- getCurrentRoute
    PageContent title headTags bodyTags <- widgetToPageContent $ do
        addCassius $(cassiusFile "page")
        addWidget headerW
        addWidget $ navW mCurrentRoute
        addWidget contents
        addWidget footerW
    hamletToRepHtml $(hamletFile "page")

navRoutes = [HomeR, AboutR, BlogR, CodeR]

navTitle :: Route ReacocardNet -> String
navTitle HomeR = "Home"
navTitle AboutR = "About"
navTitle BlogR = "Blog"
navTitle CodeR = "Code"

navW :: Maybe ReacocardNetRoute -> ReacocardNetWidget
navW mCurrentRoute = do
    let isCurrent route = Just route == mCurrentRoute
    addCassius $(cassiusFile "nav")
    addHamlet $(hamletFile "nav")

headerW :: ReacocardNetWidget
headerW = do
    addCassius $(cassiusFile "header")
    addHamlet $(hamletFile "header")

footerW :: ReacocardNetWidget
footerW = do
    addCassius $(cassiusFile "footer")
    addHamlet $(hamletFile "footer")

getHomeR = defaultLayout $ do
    setTitle "Home - Reacocard.net"
    addCassius $(cassiusFile "home")
    addHamlet $(hamletFile "home")

getAboutR = defaultLayout $ do
    setTitle "About Me - Reacocard.net"
    addCassius $(cassiusFile "about")
    addHamlet $(hamletFile "about")

getCodeR = defaultLayout $ do
    setTitle "Code - Reacocard.net"
    addCassius $(cassiusFile "code")
    addHamlet $(hamletFile "code")

getBlogR = defaultLayout $ do
    setTitle "Blog - Reacocard.net"
    addCassius $(cassiusFile "blog")
    addHamlet $(hamletFile "blog")


withReacocardNet f = toWaiApp ReacocardNet >>= f
