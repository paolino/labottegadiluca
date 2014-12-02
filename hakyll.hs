{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Prelude hiding (id,lookup)
import Control.Category (id)
import Control.Arrow ((>>>), (***), arr, second)
import Data.Monoid (mempty, mconcat, mappend)
import Data.Map (singleton, fromList,lookup)
import System.FilePath
import Data.Maybe (fromMaybe)
import Data.String
import Data.List hiding (lookup)
import Hakyll
import Hakyll.Web.Html
import Images


complete ctx y = loadAndApplyTemplate "templates/template.html" ctx y >>= relativizeUrls
canonicalizePath x = '/':x


main = hakyll $ do
    match "templates/*" $ compile templateCompiler

    match  "collaborazioni.html" $ do 
	route idRoute 
     	compile $ getResourceString >>= complete (constField "lavoro" "collaborazioni" `mappend` defaultContext)
    match  "contatti.html" $ do 
	route idRoute
    	compile $ getResourceString >>=  complete (constField "lavoro" "contatti" `mappend` defaultContext) 
    match  "lavori.html" $ do 
	route idRoute
    	compile $ getResourceString >>=  complete (constField "lavoro" "lavori" `mappend` defaultContext) 
    match "index.html" $ do
	route idRoute
	compile $ getResourceString >>= complete (constField "lavoro" "" `mappend` defaultContext)
    match "fotozilla/**/dir" $ do 
        route $ customRoute $ flip addExtension "html" . takeDirectory . toFilePath
        compile $ do 
                ctx <- getUnderlying >>= getMetadata
                q <-  (init . last . init. splitPath . toFilePath) `fmap` getUnderlying
                d <- (init . joinPath . init . splitPath . toFilePath) `fmap` getUnderlying
                let lavoro = canonicalizePath $ d </> fromMaybe "" (lookup "foto" ctx) 
                s <- takeDirectory `fmap` toFilePath `fmap` getUnderlying
                ts <- loadAll (fromGlob (s ++ "/**.jpg") .&&. hasVersion "linked")
                --ds <- loadAll (fromGlob (s ++ "/*/dir") .&&. hasVersion "dlinked")
                let ctx = mconcat 
                        [ constField "lavoro" lavoro
                        , constField "riferimento" q
                        , constField "elencoimmagini" (concatMap itemBody ts)
                        , constField "back" "/lavori.html"
                        ] `mappend` metadataField `mappend` defaultContext
                makeItem "" >>= loadAndApplyTemplate "templates/foto_elenco.html" ctx >>= complete ctx
    match "fotozilla/dir" $ do 
        route $ customRoute $ const "fotozilla.html"
        compile $ do 
                ctx <- getUnderlying >>= getMetadata
                d <- (init . joinPath . init . splitPath . toFilePath) `fmap` getUnderlying
                let lavoro = canonicalizePath $ d </> fromMaybe "" (lookup "foto" ctx) 
                s <- takeDirectory `fmap` toFilePath `fmap` getUnderlying
                ts <- loadAll (fromGlob (s ++ "/**.jpg") .&&. hasVersion "linked")
                ds <- loadAll (fromGlob (s ++ "/*/dir") .&&. hasVersion "dlinked")
                let ctx = mconcat 
                        [ constField "lavoro" lavoro
                        , constField "riferimento" "bottega"
                        , constField "elencoimmagini" (concatMap itemBody ts)
                        , constField "elencocartelle" (concatMap itemBody ds)
                        , constField "back" (canonicalizePath $ "fotozilla.html")
                        ] `mappend` metadataField `mappend` defaultContext
                makeItem "" >>= loadAndApplyTemplate "templates/foto_elenco.html" ctx >>= complete ctx
    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler
    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
    match "fotozilla/**.jpg" . version "raw" $ do
        route   idRoute
        compile $ imageResizeCompiler 500 400
    match "fotozilla/**.jpg" . version "thumbs" $ do
	route $ setExtension ".thumb.jpg"
	compile $ thumbResizeCompiler
    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler
    match "fotozilla/**.jpg" . version "linked" $ compile $ do
        let     path = field "path" (return . canonicalizePath .toFilePath . itemIdentifier)
                thumb = field "thumb" (return . canonicalizePath .flip replaceExtension "thumb.jpg" . toFilePath . itemIdentifier)
	getResourceString >>= loadAndApplyTemplate "templates/immagine.html" (path `mappend` thumb)
    match "fotozilla/**/dir" . version "dlinked" $ compile $ do
        ctx <- getUnderlying >>= getMetadata
        d <- (init . joinPath . init . splitPath . toFilePath) `fmap` getUnderlying
        let     thumb = canonicalizePath .flip replaceExtension "thumb.jpg" $ d </> fromMaybe "" (lookup "foto" ctx) 
                path = canonicalizePath .  flip addExtension "html" $ d
	getResourceString  >>= loadAndApplyTemplate "templates/immagine.html" (constField "path" path `mappend` constField "thumb" thumb `mappend` defaultContext)
    match "fotozilla/dir" . version "dlinked" $ compile $ do
        ctx <- getUnderlying >>= getMetadata
        d <- (init . joinPath . init . splitPath . toFilePath) `fmap` getUnderlying
        let     thumb = canonicalizePath .flip replaceExtension "thumb.jpg" $ d </> fromMaybe "" (lookup "foto" ctx) 
                path = canonicalizePath .  flip addExtension "html" $ d
	getResourceString  >>= loadAndApplyTemplate "templates/immagine.html" (constField "path" path `mappend` constField "thumb" thumb `mappend` defaultContext)

