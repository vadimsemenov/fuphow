{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types      #-}

module Lenses.FileSystem
       ( FS (..)
       , getFS
       , cd
       ) where

import           Control.Lens
import           System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import           System.FilePath  (splitPath, (</>))

data FS
    = Dir
          { _name     :: FilePath  -- название папки, не полный путь
          , _contents :: [FS]
          }
    | File
          { _name     :: FilePath  -- название файла, не полный путь
          }

instance Show FS where
    show = show' 0
      where
        show' offset (File n)   = spaces' offset ++ n
        show' offset (Dir n cs) = let cur = spaces' offset ++ n in
            cur ++ concatMap (("\n" ++) . show' (length cur)) cs
        spaces' n = replicate n ' '

makeLenses ''FS
makePrisms ''FS

getFS :: FilePath -> IO FS
getFS path = (,) <$> doesFileExist path <*> doesDirectoryExist path >>= \case
    (True, False) -> return $ File path
    (False, True) -> Dir (last $ splitPath path) <$>
        (listDirectory path >>= mapM (getFS . (path </>)))
    _             -> error $ "Error on path: " ++ path

{-
λ> getFS "/Users/vadim/Documents/fuphow/hw4/src"
src
   Comonads
           /Users/vadim/Documents/fuphow/hw4/src/Comonads/ProjectBuilder.hs
           /Users/vadim/Documents/fuphow/hw4/src/Comonads/Renew.hs
           /Users/vadim/Documents/fuphow/hw4/src/Comonads/RoseTree.hs
   Lenses
         /Users/vadim/Documents/fuphow/hw4/src/Lenses/Basic.hs
         /Users/vadim/Documents/fuphow/hw4/src/Lenses/FileSystem.hs
   /Users/vadim/Documents/fuphow/hw4/src/TemplateLib.hs
it :: FS
-- -}

isDir :: FS -> Bool
isDir (Dir _ _) = True
isDir _         = False

cd :: FilePath -> Traversal' FS FS
cd dir = contents . traversed . filtered (\x -> isDir x && x ^. name == dir)

{-
λ> src <- getFS "/Users/vadim/Documents/fuphow/hw4/src"
src :: FS
λ> src ^.. cd "Lenses"
[Lenses
      /Users/vadim/Documents/fuphow/hw4/src/Lenses/Basic.hs
      /Users/vadim/Documents/fuphow/hw4/src/Lenses/FileSystem.hs]
it :: [FS]
-- -}
