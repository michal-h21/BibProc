{-# LANGUAGE TypeSynonymInstances #-}

module BibProc where
import qualified Data.IntMap as M
import qualified Data.Text.Lazy as L
data BibDatabase = BibDatabase (M.IntMap BibRecord) deriving (Show,Read)


class BibProc a where
  bibImport :: a -> Int ->BibDatabase
  bibExport :: BibDatabase -> a


data BibRecord = BibRecord {
  citeKey   :: CiteKey, 
  bibTypes  :: [BibType], 
  bibFields :: [BibField]
} deriving (Show, Eq,Read)


type CiteKey = L.Text
type DatabaseKey = Int

data BibType = Article
             | Book
             | InProceedings
             | WebPage 
             | Other String
               deriving (Show,Eq,Read)

type BibTag = L.Text                        
type BibContent = L.Text
{-
data BibField = BPersonal BibTag Personal
              | BField BibTag BibContent              
              | BIdent BibTag BibContent
              | BClassify BibTag BibContent
              | BTitle BibTag BibContent 
                 deriving(Show,Eq,Read)
-}
data BibObject = BPersonal {
                       family :: BibContent,
                       given :: BibContent
                       } 
              | BInstitution BibContent
              | BTitle {
                       title :: BibContent,
                       subTitle :: BibContent
                       }
              | BField BibContent
              deriving(Show,Eq,Read)


data BibField = BibField 
                {
                  bibTag :: BibTag,
                  bibContent :: BibObject
                } 
              deriving(Show,Eq,Read)
{- data Personal = Person {
                       family :: BibContent,
                       given :: BibContent
                       } 
              | Institution String
                deriving(Show,Eq,Read)
-}
                
class BibConvert a where
  toBibRecord   :: a -> BibRecord
  fromBibRecord :: BibRecord -> a

class BibFieldConvert a where 
  toField   :: a -> BibContent
  fromField :: BibContent -> a
  toTag     :: a -> BibTag
  fromTag   :: BibTag -> a
  
instance BibFieldConvert String where
  toField = L.pack
  fromField = L.unpack
  toTag = L.pack
  fromTag = L.unpack

instance BibFieldConvert L.Text where
  toField = id
  fromField = id
  toTag = id
  fromTag = id

createBibField :: String -> BibObject -> BibField
createBibField x y = BibField (toTag x) y

--better to use filter testFn [BibField]
--example: filter (\s -> case s of (Title _ _) -> True; _ -> False) (bibFields sample)
bibLookup :: DatabaseKey -> BibDatabase -> Maybe BibRecord
bibLookup x (BibDatabase m) = M.lookup x m
--bibLookup _ _ = Nothing



--showBibFields :: BibRecord -> String
--showBibFields    (BibRecord k b fields) = "Záznam. Citekey: " ++ k 
