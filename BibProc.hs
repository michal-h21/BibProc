type BibRecord = [BibField]

data BibField = Authors [Personal]
               | Titles [TitleInfo] 
                 deriving(Show)

data Personal = Person PersonName 
              | Institution String
                deriving(Show)
                
                
type PersonName = (String,String)

data TitleInfo = Title String
               | SubTitle String
                 deriving(Show)