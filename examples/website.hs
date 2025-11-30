{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.List as List
import Haal.BlackBox
import System.Process (readProcess)
import Control.Monad ((<=<))

data Page = Home | About | CV | Links | Blogs
    deriving (Eq, Show)
data PageTag = LandingTag | AboutTag | CVTag | LinksTag | BlogsTag | NotFoundTag
    deriving (Eq, Show)

baseUrl :: String
baseUrl = "http://www.anunknown.me"

inputMap :: Page -> String
inputMap Home = "/"
inputMap About = "/about"
inputMap CV = "/education-and-work"
inputMap Links = "/links"
inputMap Blogs = "/blogs"

fetchPage :: String -> IO String
fetchPage url = readProcess "curl" ["-sL", url] ""

outputMap :: String -> PageTag
outputMap html
    | "<title>About" `List.isInfixOf` html = AboutTag
    | "<title>Education" `List.isInfixOf` html = CVTag
    | "<title>Useful Links" `List.isInfixOf` html = LinksTag
    | "<title>My Blogs" `List.isInfixOf` html = BlogsTag
    | "<title>Stefanos" `List.isInfixOf` html = LandingTag
    | otherwise = NotFoundTag

-- sul type must be parameterized by input output, which
-- I now see that is a bit silly.
newtype WebsiteSUL input output = WebsiteSUL String

instance SUL WebsiteSUL IO Page PageTag where
    step system input = do
        let url = inputMap input
        html <- fetchPage url
        let tag = outputMap html
        return (system, tag)
    reset = return

example system input = (<=<)

main :: IO ()
main = undefined
