-- | Exports simple compilers to just copy files
--
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Images (imageResizeCompiler, thumbResizeCompiler) where

import Control.Arrow ((>>^))
import System.Cmd (rawSystem, system)

import Data.Typeable (Typeable)
import Data.Binary (Binary)

import Hakyll.Core.Item
import Hakyll.Core.Writable
import Hakyll.Core.Compiler
import Hakyll.Core.Identifier

newtype Images = Images (Int,Int,FilePath)
                 deriving (Show, Eq, Ord, Binary, Typeable)

instance Writable Images where
    write dst (Item _ (Images (w,h,src))) = let r = "convert -resize " ++ show w ++ "x" ++ show h ++ " \"" ++ src ++ "\" \"" ++  dst ++ "\""
	in system r  >> return ()
imageResizeCompiler :: Int -> Int -> Compiler (Item Images)
imageResizeCompiler w h = getUnderlying >>= \y -> return (Item y $ Images (w,h,toFilePath y))


newtype Thumbs = Thumbs FilePath
                 deriving (Show, Eq, Ord, Binary, Typeable)

instance Writable Thumbs where
    write dst (Item _ (Thumbs src)) = let r = "./makethumb " ++ " \"" ++ src ++ "\" \"" ++  dst ++ "\""
	in system r  >> return ()

thumbResizeCompiler ::  Compiler (Item Thumbs)
thumbResizeCompiler = getUnderlying >>= \y ->  return (Item y $ Thumbs (toFilePath y))

