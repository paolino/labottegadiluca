{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (***), arr, second)
import Data.Monoid (mempty, mconcat, mappend)
import Data.Map (singleton, fromList)
import System.FilePath

import Hakyll
import Images


complete = applyTemplateCompiler "templates/template.html" >>> relativizeUrlsCompiler

lavori s = do
    match (parseGlob $ s ++ ".html") $ route idRoute
    create (parseIdentifier $  s ++ ".html") $ constA mempty
	>>> requireAll (parseGlob ("photos/" ++ s ++ "/**.jpg") `mappend` inGroup (Just "linked")) 
		(\p (ts :: [Page String]) -> setField "lavoro" s $ setField "elencoimmagini" (concatMap pageBody $ ts) p )
	>>> applyTemplateCompiler "templates/foto_elenco.html"
	>>> complete


main :: IO ()
main = hakyll $ do

    match "templates/*" $ compile templateCompiler

    match  "contatti.html" $ do 
	route idRoute
     	compile $ readPageCompiler >>> arr (setField "scelta" "contatti") >>> complete
    match  "collaborazioni.html" $ do 
	route idRoute 
     	compile $ readPageCompiler >>> arr (setField "scelta" "collaborazioni") >>> complete
    match  "lavori.html" $ do 
	route idRoute
     	compile $ readPageCompiler >>> arr (setField "scelta" "lavori") >>> complete

    match "index.html" $ do
	route idRoute
	compile $ readPageCompiler >>> complete

    _  <- mapM lavori ["armadi","tavoli","bagni","cucine","varie","librerie","progetti"]
 
    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
    group "raw" . match "photos/**.jpg" $ do
        route   idRoute
        compile $ imageResizeCompiler 500 400
    group "thumbs" . match "photos/**.jpg" $ do
	route $ setExtension ".thumb.jpg"
	compile $ thumbResizeCompiler
    group "linked" . match "photos/**.jpg" $ compile $
	arr (fromMap . (\s -> fromList [("path",s),("thumb",replaceExtension s "thumb.jpg")]) . unResource) 
	>>> applyTemplateCompiler "templates/immagine.html" 
    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler
