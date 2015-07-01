{-# LANGUAGE OverloadedStrings #-}
module Main where

import Views.Site

import Control.Monad.Trans
import Data.List
import Data.Ord
import Data.Time.Clock (UTCTime)
import Network.Wai.Middleware.Static
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger
import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Web.Spock.Safe

main :: IO ()
main = do
    h <- fileHandler "server.log" INFO >>= \lh -> return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger "blog.requests" (addHandler h)
    updateGlobalLogger "blog.requests" (setLevel INFO)
    runSpock 80 $ spockT id $ (appMiddleware >> appRoutes)

appMiddleware :: SpockT IO ()
appMiddleware = middleware (staticPolicy (noDots >-> addBase "static"))

appRoutes :: SpockT IO ()
appRoutes = do
    get root $ do
        r <- request
        liftIO $ infoM "blog.requests" $ show r
        contents <- liftIO $ getNewestFile "Views/posts/"
        renderView contents
    get "about" $ renderFileView "static/about"
    get "favicon.ico" $ do
        file "image/x-icon" "favicon.ico"
    get (root <//> var) $ 
        \post -> renderFileView ("Views/posts/" ++ post)

writeLog :: ActionT IO ()
writeLog = do
    r <- request
    liftIO $ infoM "blog.requests" $ show r

renderFileView :: String -> ActionT IO ()
renderFileView filename = do
    writeLog
    contents <- liftIO $ readFile $ filename ++ ".html"
    renderView contents

renderView :: String -> ActionT IO ()
renderView contents = do
    sidebar <- liftIO $ getDirectoryContentsByDate "Views/posts/"
    blaze $ siteView blog (homeView contents) (sidebarView sidebar)

blog :: ViewDetails
blog = ViewDetails { getTitle = "James Ekstrom", getDesc = "James Ekstrom's Blog" }

blaze :: MonadIO m => Html -> ActionT m a
blaze = lazyBytes . renderHtml

getNewestFile :: String -> IO String
getNewestFile path = do
    timedFiles <- getFileTimes path
    readFile $ snd $ maximumBy (comparing fst) timedFiles

getDirectoryContentsByDate :: String -> IO [String]
getDirectoryContentsByDate path = do
    timedFiles <- getFileTimes path
    return $ getNamesSortedByDate timedFiles

getFileTimes :: String -> IO [(UTCTime, String)]
getFileTimes path = do
    dirContents <- getDirectoryContents path
    let files = map (path ++) $ filterDirContents dirContents
    times <- mapM getModificationTime files
    return $ zip times files

filterDirContents :: [String] -> [String]
filterDirContents = filter (\d -> d /= "." && d /= "..")

getNamesSortedByDate :: [(UTCTime, String)] -> [String]
getNamesSortedByDate timedFiles = map (\(t, p) -> getFileNameNoExtension p) $ sortBy (comparing fst) timedFiles

getFileNameNoExtension :: String -> String
getFileNameNoExtension = dropExtension . takeFileName
