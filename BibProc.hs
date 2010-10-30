data BibRecord = BibRecord CiteKey BibType [BibField]

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

data Personal = Person PersonName 
              | Institution String
                deriving(Show)
                
                
type PersonName = (String,String)

--data TitleInfo = Title String
--               | SubTitle String
--                 deriving(Show)
                         
--showBibFields :: BibRecord -> String
--showBibFields [] = []
--showBibFields Authors:rest = "autor " ++ showBibFields rest
--showBibFields Titles:rest = "titulek " ++ showBibFields rest
--showBibFields _:rest = "nezname pole pole" ++ showBibFields rest