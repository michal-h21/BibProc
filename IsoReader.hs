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
             
type IsoRecord = [IsoField]
type IsoField = (L.Text,L.Text)


instance BibProc IsoFile where
         bibImport = processIsoFile 
         
         
loadIsoFile fileName = do
  cont <- LIO.readFile fileName
  return $ IsoFile fileName cont
  
fileNames = do
            names <- getArgs  
            return $ case names of 
              [] -> ["EKO07_new.ISO"]
              (x:xs) -> names 
              
              
--printFiles :: -> [String] -> IO 

--ToDo!!!!!! Předělat
              
processIsoFile :: IsoFile -> Int -> BibDatabase
processIsoFile f i = BibDatabase M.empty
--processIsoFile f i = BibDatabase M.fromList zip [i..] $ parseISO f

--ToDO: Parser
parseISO :: L.Text -> [IsoRecord]
parseISO file = --[("OOO",BibRecord "ooo" [Article] [])]
  map isoFields $ spl file
  where
    spl = L.split $ splitString
    splitString = L.pack "##"
    fieldSplit = L.pack "#"
    isoFields s = 
      let 
        clean = L.replace (L.pack "\n") (L.pack "") s
        header:fields = L.split fieldSplit clean
        dir = L.chunksOf 12 $ snd (L.splitAt 25 header)
      in
      zipWith (\x y -> (x,y)) dir fields
      --(dir, fields) 

main = do
  names <- fileNames 
  putStrLn $ intercalate "\n" names 
  iso <- mapM (loadIsoFile) names
  --map (putStrLn $ L.length . contents) iso 
  
  putStrLn "Konec"
  