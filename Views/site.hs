{-# LANGUAGE OverloadedStrings #-}

module Views.Site where

import Control.Monad (forM_)
import qualified Data.Text as T
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

data ViewDetails = ViewDetails 
    { getTitle :: T.Text
    , getDesc :: T.Text
    }

siteView :: ViewDetails -> H.Html -> H.Html -> H.Html
siteView viewDetails body sidebar = html $ do
    H.head $ do
        H.title (H.toHtml $ getTitle viewDetails)
        H.meta ! A.charset "utf-8"
        H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
        H.link ! A.href "//maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css" ! A.rel "stylesheet"
        H.link ! A.href "assets/css/blog.css" ! A.rel "stylesheet"
    H.body $ do
        H.div ! A.class_ "blog-masthead" $
            do
                H.div ! A.class_ "blog-title" $ H.html "James Ekstrom"
                H.div ! A.class_ "container" $
                    H.nav ! A.class_ "blog-nav" $
                        do 
                            H.a ! A.class_ "blog-nav-item" ! A.href "/" $ H.html "Home"
                            H.a ! A.class_ "blog-nav-item" ! A.href "/about" $ H.html "About"
        H.div ! A.class_ "container" $
            do
                H.div ! A.class_ "row" $
                    do
                        H.div ! A.class_ "col-sm-8 blog-main" $ body
                        H.div ! A.class_ "col-sm-3 col-sm-offset-1 blog-sidebar" $ sidebar

sidebarView :: [String] -> H.Html
sidebarView filenames = 
    H.div ! A.class_ "sidebar-module" $
        do
            H.div ! A.class_ "h3" $ H.html "Archives"
            H.ol ! A.class_ "list-unstyled" $
                forM_ filenames (\fn -> H.li $ H.a ! A.href (H.stringValue fn) $ H.toHtml fn)


homeView :: String -> H.Html
homeView contents = H.html $ H.div $ preEscapedString contents