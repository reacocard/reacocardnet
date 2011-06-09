{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving #-}


module Controller where


import Data.Time
import System.Locale(defaultTimeLocale)
import ReacocardNet
import Yesod hiding (defaultLayout)
import Database.Persist
import Database.Persist.GenericSql
import Text.Pandoc
import Settings

mkYesodDispatch "ReacocardNet" resourcesReacocardNet

getHomeR :: Handler RepHtml
getHomeR = panedLayout 50 left right "Home - Reacocard.net"
    where
        left  = addHamlet $(hamletFile "blocks/recentblogposts")
        right = addHamlet $(hamletFile "blocks/minibio")

getAboutR :: Handler RepHtml
getAboutR = defaultLayout $ do
    setTitle "About - Reacocard.net"
    addHamlet $(hamletFile "blocks/aboutaren")
    addHamlet $(hamletFile "blocks/aboutreacocardnet")

getCodeR :: Handler RepHtml
getCodeR = panedLayout 50 left right "Code - Reacocard.net"
    where
        left  = do
            addHamlet $(hamletFile "codeblocks/exaile")
            addHamlet $(hamletFile "codeblocks/hcompmgr")
            addHamlet $(hamletFile "codeblocks/hncurses")
        right = do
            addHamlet $(hamletFile "codeblocks/reacocardnet")
            addHamlet $(hamletFile "codeblocks/dotfiles")
            addHamlet $(hamletFile "codeblocks/rockboxdbpython")

getTestR :: Handler RepHtml
getTestR = panedLayout 50 left right "Test"
    where
        left  = addHamlet $(hamletFile "blocks/recentblogposts")
        right = addHamlet $(hamletFile "blocks/minibio")

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
