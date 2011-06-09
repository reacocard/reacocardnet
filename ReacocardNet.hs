{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module ReacocardNet where

import Data.Time
import System.Locale(defaultTimeLocale)

import Yesod hiding (defaultLayout)
import Database.Persist
import Database.Persist.GenericSql
import Text.Pandoc
import Data.Text

import Settings

--import Blocks.Test1

data ReacocardNet = ReacocardNet {
    connPool :: Settings.ConnectionPool
    }


share [mkPersist, mkMigrate "migrateAll"] [persist|
    BlogPost
        title String
        body String     -- markdown
        posted UTCTime Desc Gt Ge Lt Le Eq
|]

mkYesodData "ReacocardNet" [parseRoutes|
    / HomeR GET
    /about AboutR GET
    /code CodeR GET

    /blog BlogR GET
    /blog/post/#BlogPostId BlogPostR GET

    /test TestR GET
|]



type Handler = GHandler ReacocardNet ReacocardNet

type Widget = GWidget ReacocardNet ReacocardNet

instance Yesod ReacocardNet where
    approot _ = ""

instance YesodPersist ReacocardNet where
    type YesodDB ReacocardNet = SqlPersist
    runDB db = liftIOHandler
             $ fmap connPool getYesod >>= Settings.runConnectionPool db

defaultLayout :: Widget () -> Handler RepHtml
defaultLayout contents = do
    mCurrentRoute <- getCurrentRoute
    PageContent title headTags bodyTags <- widgetToPageContent $ do
        addCassius $(cassiusFile "page")
        addHamletHead [hamlet|<script src="http://html5shiv.googlecode.com/svn/trunk/html5.js">|]
        addWidget headerW
        addWidget $ navW mCurrentRoute
        addWidget contents
        addWidget footerW
    hamletToRepHtml $(hamletFile "page")

panedLayout :: Int -> Widget () -> Widget () -> Html -> Handler RepHtml
panedLayout percent left right title = defaultLayout $ do
    let lpercent = show percent
    let rpercent = show $ (100 :: Int) - percent
    setTitle title
    addWidget [hamlet|
    <div .panedlayout .panedlayout_left_#{lpercent}
        ^{left}
    <div .panedlayout .panedlayout_right_#{rpercent}
        ^{right}
|]
    addCassius [cassius|
.panedlayout
    float: left
    margin: 0
    padding: 0
.panedlayout_left_#{lpercent}
    width: #{lpercent}%
.panedlayout_right_#{rpercent}
    width: #{rpercent}%
|]


navRoutes :: [ReacocardNetRoute]
navRoutes = [HomeR, AboutR, BlogR, CodeR]

navTitle :: Route ReacocardNet -> Text
navTitle HomeR = "Home"
navTitle AboutR = "About"
navTitle BlogR = "Blog"
navTitle CodeR = "Code"
navTitle _ = "ERROR - Route unknown!"

navIsCurrent :: Maybe (Route ReacocardNet) -> Route ReacocardNet -> Bool
navIsCurrent Nothing _                  = False
navIsCurrent (Just (BlogPostR _)) BlogR = True
navIsCurrent (Just current) route       = route == current

navW :: Maybe ReacocardNetRoute -> Widget ()
navW mCurrentRoute = do
    let isCurrent = navIsCurrent mCurrentRoute
    addCassius $(cassiusFile "nav")
    addHamlet $(hamletFile "nav")

headerW :: Widget ()
headerW = do
    addCassius $(cassiusFile "header")
    addHamlet $(hamletFile "header")

footerW :: Widget ()
footerW = do
    addCassius $(cassiusFile "footer")
    addHamlet $(hamletFile "footer")


