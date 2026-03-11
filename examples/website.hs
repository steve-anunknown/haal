{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import qualified Data.List as List
import Haal.BlackBox
import Haal.EquivalenceOracle.WMethod (WMethodConfig (..), mkWMethod)
import Haal.Experiment
import Haal.Learning.LMstar
import System.Process (readProcess)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Page = Home | About | CV | Links | Blogs | WrongUrl
    deriving (Eq, Show, Ord, Enum, Bounded)

data PageTag
    = LandingTag
    | AboutTag
    | CVTag
    | LinksTag
    | BlogsTag
    | NotFoundTag
    deriving (Eq, Show, Ord, Enum, Bounded)

data WebsiteSUL i o = WebsiteSUL
    { baseUrl :: String
    , currentUrl :: String -- current path, e.g. "/about"
    , toPath :: i -> String
    , fromHtml :: String -> o
    , notFound :: o
    }

-- Fetch + HTML processing
--------------------------------------------------------------------------------

fetch :: String -> IO String
fetch url = readProcess "curl" ["-sL", url] ""

--------------------------------------------------------------------------------
-- Input and output abstraction
--------------------------------------------------------------------------------

inputMap :: Page -> String
inputMap Home = "/"
inputMap About = "/about"
inputMap CV = "/education-and-work"
inputMap Links = "/links"
inputMap Blogs = "/blogs"
inputMap WrongUrl = "/garbage"

outputMap :: String -> PageTag
outputMap html
    | "<title>About" `List.isInfixOf` html = AboutTag
    | "<title>Education" `List.isInfixOf` html = CVTag
    | "<title>Useful Links" `List.isInfixOf` html = LinksTag
    | "<title>My Blogs" `List.isInfixOf` html = BlogsTag
    | "<title>Stefanos" `List.isInfixOf` html = LandingTag
    | otherwise = NotFoundTag

-- >>> html <- fetch "http://www.anunknown.me/blogs"
-- >>> outputMap html
-- BlogsTag

--------------------------------------------------------------------------------
-- SUL instance
--------------------------------------------------------------------------------

instance SUL WebsiteSUL IO where
    reset sul = pure sul{currentUrl = "/"}

    step sul input = do
        if currentUrl sul /= "/garbage"
            then do
                let suffix = toPath sul input
                html <- fetch (baseUrl sul ++ suffix)
                let out = fromHtml sul html
                    sul' = sul{currentUrl = suffix}
                pure (sul', out)
            else pure (sul, notFound sul)

--------------------------------------------------------------------------------
-- Experiment setup
--------------------------------------------------------------------------------

learner = mkLMstar Star
teacher = mkWMethod (WMethodConfig 2)
exper = experiment learner teacher

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "Learning Experiment"
    putStrLn "==================="
    putStrLn "System Under Learning: my personal website"
    let _baseUrl = "http://www.anunknown.me"
        _currentUrl = "/"
    let website =
            WebsiteSUL
                { baseUrl = _baseUrl
                , currentUrl = _currentUrl
                , toPath = inputMap
                , fromHtml = outputMap
                , notFound = NotFoundTag
                } ::
                WebsiteSUL Page PageTag
    (model, _) <- runExperimentT exper website
    putStrLn $ "Learned Model: " ++ show model
