{-# LANGUAGE TypeSynonymInstances #-}
module IsoReader where
import BibProc
import System.Environment (getArgs)
--import Control.Monad
import Data.List (intercalate)
import qualified Data.IntMap as M
--import qualified Data.ByteString as Bs
--import qualified Data.ByteString.UTF8 as B
import qualified Data.Map as Map
import qualified Data.IntSet as S
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LIO

data IsoFile = IsoFile {
  fileName :: String,
  contents :: L.Text
  } deriving (Show,Read,Eq)
             
type IsoRecord = [IsoField]
type IsoField = (L.Text,L.Text)


instance BibProc IsoFile where
         bibImport   = processIsoFile 
         bibExport x = IsoFile {fileName ="", contents = (L.pack "")}

instance BibConvert IsoRecord where
  fromBibRecord r = []
  toBibRecord iso = BibRecord {
    citeKey = L.pack ""
    , bibTypes = [Article]
    , bibFields = 
      (map 
       (\x -> isoToBibField (L.unpack (fst x)) (snd x)) 
       iso)
    } 
         
loadIsoFile:: String -> IO IsoFile         
loadIsoFile fileName = do
  cont <- LIO.readFile fileName
  return $ IsoFile fileName cont
  
fileNames = do
            names <- getArgs  
            return $ case names of 
              [] -> ["EKO07_new.ISO"]
              (x:xs) -> names 
              
              
--printFiles :: -> [String] -> IO 
isoToBibField :: String -> L.Text -> BibField
isoToBibField "100" x = createBibField "title" (BTitle (toField x) (toField ""))
isoToBibField "010" x = createBibField "author" (BPersonal (toField $ head au) (toField $ L.intercalate (L.pack ", ") $ tail au )  where
  au = L.split(L.pack ", ") x
isoToBibField c x = createBibField c (BField x)
{-
isoToBibField "100" x = BTitle  (L.pack "title") x             
isoToBibField "010" x = BPersonal (L.pack "author") Person {
    family = head au
  , given  =  L.intercalate (L.pack ", ") $ tail au
  }
  where
    au = L.split (L.pack ", ") x
    
isoToBibField t x = BField (L.pack t) x
-}                    
         

processIsoFile :: IsoFile -> Int -> BibDatabase
--processIsoFile f i = BibDatabase M.empty
processIsoFile f i = BibDatabase $ M.fromList $ zip [i..] (map toBibRecord $ parseISO $ contents f)

--ToDO: Parser
parseISO :: L.Text -> [IsoRecord]
parseISO file = --[("OOO",BibRecord "ooo" [Article] [])]
  map isoFields $ spl file
  where
    spl = L.split $ splitString
    splitString = L.pack "##"
    
isoFields :: L.Text -> IsoRecord
isoFields s = 
      let 
        clean = L.replace (L.pack "\n") (L.pack "") s
        header:fields = L.split fieldSplit clean
        dir = L.chunksOf 12 $ snd (L.splitAt 25 header)
        fieldSplit = L.pack "#"
      in
      zipWith (\x y -> ((L.take 3 x),y)) dir fields
      --(dir, fields) 

--buildIndex x d = M.foldWithKey indexBibRecord d x where
--  indexBibRecord k v res = map     Map.insertWith S.union 

main = do
  names <- fileNames 
  putStrLn $ intercalate "\n" names 
  iso <- mapM (loadIsoFile) names
  --map (putStrLn $ L.length . contents) iso 
  
  putStrLn "Konec"
  