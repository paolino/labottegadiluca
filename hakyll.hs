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


main :: IO ()
main = hakyll $ do

    match "templates/*" $ compile templateCompiler

    match  "contatti.html" $ do 
	route idRoute
     	compile $ pageCompiler >>> arr (setField "scelta" "contatti") >>> complete
    match  "collaborazioni.html" $ do 
	route idRoute 
     	compile $ pageCompiler >>> arr (setField "scelta" "collaborazioni") >>> complete
    match  "lavori.html" $ do 
	route idRoute
     	compile $ pageCompiler >>> arr (setField "scelta" "lavori") >>> complete

    match "bagni.html" $ route idRoute
    create "bagni.html" $ constA mempty
	>>> requireAll ("photos/bagni/**.jpg" `mappend` inGroup (Just "linked")) 
		(\p (ts :: [Page String]) -> setField "elencoimmagini" (concatMap pageBody $ ts) p )
	>>> applyTemplateCompiler "templates/foto_elenco.html"
	>>> complete


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
	compile $ imageResizeCompiler 75 75
    group "linked" . match "photos/**.jpg" $ compile $
	arr (fromMap . (\s -> fromList [("path",s),("thumb",replaceExtension s "thumb.jpg")]) . unResource) 
	>>> applyTemplateCompiler "templates/immagine.html" 
    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler
