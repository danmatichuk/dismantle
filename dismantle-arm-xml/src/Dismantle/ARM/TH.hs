module Dismantle.ARM.TH where

import qualified Data.Foldable as F
import           Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Traversable as T
import qualified Language.Haskell.TH as TH

import           System.Directory (withCurrentDirectory, listDirectory, makeAbsolute)
import           System.FilePath.Posix (isExtensionOf)

import qualified Text.XML.Light as X

import qualified Dismantle.ARM as DA
import qualified Dismantle.Tablegen.TH as DTH

genISA ::  DA.ISA -> FilePath -> FilePath -> FilePath -> TH.DecsQ
genISA isa dirPath encIndexFile logFile = do
  (desc, xmlFiles) <- TH.runIO $ do
    logFile' <- makeAbsolute logFile
    putStrLn $ "Dismantle.ARM.TH log: " ++ logFile'
    withCurrentDirectory dirPath $ do
      files <- sort <$> listDirectory "."
      let xmlFiles = filter ("xml" `isExtensionOf`) files
      desc <- DA.loadXML (DA.isaName isa) xmlFiles encIndexFile logFile'
      return (desc, xmlFiles)
  TH.runIO $ putStrLn "Successfully generated ISA description."
  DTH.genISADesc isa desc (((dirPath ++ "/") ++) <$> xmlFiles)
