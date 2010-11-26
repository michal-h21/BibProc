module BibProc where
import qualified Data.Map as M
import qualified Data.Text.Lazy as L
data BibDatabase = BibDatabase (M.Map CiteKey BibRecord) deriving (Show,Read)


class BibProc a where
  bibImport :: a -> Int ->BibDatabase
  bibExport :: BibDatabase -> a


data BibRecord = BibRecord {
  citeKey   :: CiteKey, 
  bibTypes  :: [BibType], 
  bibFields :: [BibField]
} deriving (Show, Eq,Read)


type CiteKey = String

data BibType = Article
             | Book
             | InProceedings
             | WebPage 
             | Other String
               deriving (Show,Eq,Read)

type BibTag = L.Text                        
type BibContent = L.Text
data BibField = BPersonal BibTag Personal
              | BField BibTag BibContent              
              | BIdent BibTag BibContent
              | BClassify BibTag BibContent
              | BTitle BibTag BibContent 
                 deriving(Show,Eq,Read)

data Personal = Person {
                       family :: BibContent,
                       given :: BibContent
                       } 
              | Institution String
                deriving(Show,Eq,Read)
                
class BibConvert a where
  toBibRecord   :: a -> BibRecord
  fromBibRecord :: BibRecord -> a

--better to use filter testFn [BibField]
--example: filter (\s -> case s of (Title _ _) -> True; _ -> False) (bibFields sample)
bibLookup :: CiteKey -> BibDatabase -> Maybe BibRecord
bibLookup x (BibDatabase m) = M.lookup x m
--bibLookup _ _ = Nothing

showBibFields :: BibRecord -> String
showBibFields    (BibRecord k b fields) = "Záznam. Citekey: " ++ k 

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
               where lhs = filter (< x) xs
                     rhs = filter (>= x) xs
                     