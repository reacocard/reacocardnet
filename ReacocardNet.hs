{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module ReacocardNet where

import Data.Time
import System.Locale(defaultTimeLocale)

import Yesod hiding (defaultLayout)
import Database.Persist
import Database.Persist.GenericSql
import Text.Pandoc

import Settings

data ReacocardNet = ReacocardNet {
    connPool :: Settings.ConnectionPool
    }

data BlogEntry = BlogEntry

share [mkPersist, mkMigrate "migrateAll"] [persist|
    BlogPost
        title String
        body String     -- markdown
        posted UTCTime Desc Gt Ge Lt Le Eq
|]

mkYesod "ReacocardNet" [parseRoutes|
    / HomeR GET
    /about AboutR GET
    /code CodeR GET

    /blog BlogR GET
    /blog/post/#BlogPostId BlogPostR GET
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
        addWidget headerW
        addWidget $ navW mCurrentRoute
        addWidget contents
        addWidget footerW
    hamletToRepHtml $(hamletFile "page")

navRoutes :: [ReacocardNetRoute]
navRoutes = [HomeR, AboutR, BlogR, CodeR]

navTitle :: Route ReacocardNet -> String
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

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
    setTitle "Home - Reacocard.net"
    addCassius $(cassiusFile "home")
    addHamlet $(hamletFile "home")

getAboutR :: Handler RepHtml
getAboutR = defaultLayout $ do
    setTitle "About Me - Reacocard.net"
    addCassius $(cassiusFile "about")
    addHamlet $(hamletFile "about")

getCodeR :: Handler RepHtml
getCodeR = defaultLayout $ do
    setTitle "Code - Reacocard.net"
    addCassius $(cassiusFile "code")
    addHamlet $(hamletFile "code")


blogPostDateFormat :: String -> BlogPost -> String
blogPostDateFormat fmt post = formatTime defaultTimeLocale fmt $ blogPostPosted post

blogPostDateFormatPubdate :: BlogPost -> String
blogPostDateFormatPubdate = blogPostDateFormat "%F"

blogPostDateFormatPubdatestr :: BlogPost -> String
blogPostDateFormatPubdatestr = blogPostDateFormat "%B %e, %Y"

blogPostBodyHtml :: BlogPost -> Html
blogPostBodyHtml post = preEscapedString
                    $ writeHtmlString defaultWriterOptions 
                    $ readMarkdown defaultParserState 
                    $ blogPostBody post

getBlogPostR :: BlogPostId -> Handler RepHtml
getBlogPostR postid = do
    mPost <- runDB $ get postid
    case mPost of
        Nothing -> defaultLayout $ do
            setTitle "Not Found - Reacocard.net"
            addHamlet [hamlet|Resource not found|]
        Just post -> defaultLayout $ do
            setTitle $ toHtml $ (blogPostTitle post) ++ " - Blog - Reacocard.net"
            addCassius $(cassiusFile "blog")
            addHamlet $(hamletFile "blognav")
            addHamlet $(hamletFile "blogpost")

getBlogR :: Handler RepHtml
getBlogR = do
    posts <- runDB $ selectList
        []
        [BlogPostPostedDesc] 
        0 0
    defaultLayout $ do
        setTitle "Blog - Reacocard.net"
        addCassius $(cassiusFile "blog")
        addHamlet $(hamletFile "blognav")
        mapM_ (\r@(postid, post) -> addHamlet $(hamletFile "blogpost")) posts



withReacocardNet :: (Application -> IO a) -> IO a
withReacocardNet f = Settings.withConnectionPool $ \pool -> do
    runConnectionPool (runMigration migrateAll) pool
    let h = ReacocardNet pool
    toWaiApp h >>= f
