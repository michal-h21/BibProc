module BibProc where
data BibRecord = BibRecord {
  citeKey   :: CiteKey, 
  bibTypes  :: [BibType], 
  bibFields :: [BibField]
} deriving (Show, Eq,Read)

class BibProc a where
  bibImport :: a -> BibRecord
  bibExport :: BibRecord -> a


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
                
                
type PersonName = (String,String)


sample = BibRecord "kkkOi" [Article] [
  Title "Titulek" "Podtitulek", 
  Authors [
    Person "pepa" "novák",
    Person "Jirka" "Kájínek"
    ]
  ]

--better to use filter testFn [BibField]
--example: filter (\s -> case s of (Title _ _) -> True; _ -> False) (bibFields sample)
fetchField :: (BibField -> BibField ->  Bool) -> BibField  -> [BibField] -> Maybe BibField
fetchField _ _ [] = Nothing
fetchField fn t (x:xs) =  if (fn x t) 
                          then Just x
                          else fetchField fn t xs     

fieldTest "title" = (\s -> case s of (Title _ _) -> True; _ -> False)
fieldTest "author" = (\s -> case s of (Authors _ ) -> True; _ -> False)
fieldTest _ = (\s -> False)

testAuthor :: BibField -> BibField -> Bool
testAuthor m n | m == n = True
testAuthor _ _ = False          -- 
--fetchField (_:xs) = fetchField xs

showBibFields :: BibRecord -> String
showBibFields    (BibRecord k b fields) = "Záznam. Citekey: " ++ k 
