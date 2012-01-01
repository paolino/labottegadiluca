-- | Exports simple compilers to just copy files
--
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Images (imageResizeCompiler, thumbResizeCompiler) where

import Control.Arrow ((>>^))
import System.Cmd (rawSystem, system)

import Data.Typeable (Typeable)
import Data.Binary (Binary)

import Hakyll.Core.Resource
import Hakyll.Core.Writable
import Hakyll.Core.Compiler
import Hakyll.Core.Identifier

newtype Images = Images (Int,Int,FilePath)
                 deriving (Show, Eq, Ord, Binary, Typeable)

instance Writable Images where
    write dst (Images (w,h,src)) = let r = "convert -resize " ++ show w ++ "x" ++ show h ++ " \"" ++ src ++ "\" \"" ++  dst ++ "\""
	in system r  >> return ()

imageResizeCompiler :: Int -> Int -> Compiler Resource Images
imageResizeCompiler w h = getIdentifier >>^ \y ->  Images (w,h,toFilePath y)


newtype Thumbs = Thumbs FilePath
                 deriving (Show, Eq, Ord, Binary, Typeable)

instance Writable Thumbs where
    write dst (Thumbs src) = let r = "./makethumb " ++ " \"" ++ src ++ "\" \"" ++  dst ++ "\""
	in system r  >> return ()

thumbResizeCompiler ::  Compiler Resource Thumbs
thumbResizeCompiler = getIdentifier >>^ \y ->  Thumbs (toFilePath y)
