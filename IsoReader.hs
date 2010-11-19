module IsoReader where
import BibProc
import System.Environment (getArgs)
import Control.Monad
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.ByteString as Bs
import qualified Data.ByteString.UTF8 as B
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LIO

data IsoFile = IsoFile {
  fileName :: String,
  contents :: L.Text
  } deriving (Show,Read,Eq)
             
instance BibProc IsoFile where
         bibImport = processIsoFile 
         bibExport = empty
         
         
loadIsoFile fileName = do
  contents <- LIO.readFile fileName
  return $ IsoFile fileName contents
  
fileNames = do
            names <- getArgs  
            return $ case names of 
              [] -> ["EKO07_new.ISO"]
              (x:xs) -> names 
              
              
--printFiles :: -> [String] -> IO 

processIsoFile :: IsoFile -> Int -> BibDatabase
processIsoFile f i = BibDatabase M.empty

main = do
  names <- fileNames 
  putStrLn $ intercalate "\n" names 
  iso <- mapM (loadIsoFile) names
  --map (putStrLn $ L.length . contents) iso 
  
  putStrLn "Konec"
  