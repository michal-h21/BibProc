module BibProc where
data BibRecord = BibRecord {
  citeKey   :: CiteKey, 
  bibType   :: BibType, 
  bibFields :: [BibField]
} deriving (Show)


type CiteKey = String

data BibType = Article
             | Book
             | InProceedings
             | WebPage 
             | Other String
               deriving (Show)
data BibField = Authors [Personal]
               | Title String String 
                 deriving(Show)

data Personal = Person {
                       familyName :: String,
                       givenName :: String
                       } 
              | Institution String
                deriving(Show)
                
                
type PersonName = (String,String)


sample = BibRecord "kkkOi" Article [
  Title "Titulek" "Podtitulek", 
  Authors [
    Person "pepa" "novák",
    Person "Jirka" "Kájínek"
    ]
  ]
--ToDO: Pass test function
fetchField :: (BibField -> Bool) -> [BibField] -> Maybe BibField
fetchField _ [] = Nothing
fetchField fn (x:xs) =  if (fn x) 
                        then Just x
                        else fetchField fn xs     

testAuthor :: BibField -> Bool
testAuthor (Authors _) = True
testAuthor _ = False
--fetchField (_:xs) = fetchField xs

showBibFields :: BibRecord -> String
showBibFields    (BibRecord k b fields) = "Záznam. Citekey: " ++ k 
