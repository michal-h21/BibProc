module BibProc where
import qualified Data.Map as M

data BibDatabase = BibDatabase (M.Map CiteKey BibRecord)


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
data BibField = Authors [Personal]
               | Title String String 
                 deriving(Show,Eq,Read)

data Personal = Person {
                       familyName :: String,
                       givenName :: String
                       } 
              | Institution String
                deriving(Show,Eq,Read)
                
class BibConvert a where
  toBibRecord   :: a -> BibRecord
  fromBibRecord :: BibRecord -> a

sample = BibRecord "kkkOi" [Article] [
  Title "Titulek" "Podtitulek", 
  Authors [
    Person "pepa" "novák",
    Person "Jirka" "Kájínek"
    ]
  ]
(.-) b c = "primka z " ++ b ++ " do " ++ c
(-.) b c = "krivka z " ++ b ++ " do " ++ c
infixr 5 .- 
infixr 5 -.
--better to use filter testFn [BibField]
--example: filter (\s -> case s of (Title _ _) -> True; _ -> False) (bibFields sample)
fieldTest "title" = (\s -> case s of (Title _ _) -> True; _ -> False)
fieldTest "author" = (\s -> case s of (Authors _ ) -> True; _ -> False)
fieldTest _ = (\s -> False)

showBibFields :: BibRecord -> String
showBibFields    (BibRecord k b fields) = "Záznam. Citekey: " ++ k 

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
               where lhs = filter (< x) xs
                     rhs = filter (>= x) xs