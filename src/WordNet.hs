module WordNet
  ( parseFiles
  , catFromChar
  , wdSynsets
  , wdWordSenses
  , wdWords
  , wwRelations
  , wwCat
  , wwForm
  , wwSynset
  , wsCat
  , wsGloss
  , buildDict
--  , buildDict'
  , Dictionary
  , WordSense
  , Category (Noun,Verb,Adjective,Adverb)
  , Relation
    ( Identity
    , Synonym      , Antonym
    , Hypernym     , Hyponym
    , InstHypernym , InstHyponym
    , MembHolonym  , MembMeronym
    , PartHolonym  , PartMeronym
    , SubsHolonym  , SubsMeronym
    , TopicDomain  , TopicMember
    , RegionDomain , RegionMember
    , UsageDomain  , UsageMember
    , Head         , Satellite
    , Attribute    , DerivRelated , SeeAlso
    , Entailment   , Cause        , VerbGroup
    , Participle   , Pertainym    , DerivFrom
    )
  ) where

import Control.Applicative hiding (many)
import Control.Arrow (first)
import Control.Monad (liftM,liftM2)
import Data.Array
import Data.Char (isSpace,toLower)
import Data.List (delete,elemIndex,groupBy,nub,nubBy,sort)
import Data.Maybe (catMaybes,fromMaybe)
import Numeric
import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import qualified Data.Set as S

-- First some applicative stuff to make working with parser output nicer.
instance Applicative (Either a) where
  pure = Right
  Left f  <*> _ = Left f
  Right f <*> x = fmap f x

x <++> y = (++) <$> x <*> y

-- Our WordNet file format error messages; these should never occur.
relationError code = error $
  "Error: " ++ code ++ " is not a valid relation identifier."

synsetError cat off = error $
  "Error: Cannot find synset for " ++ show cat ++
  " at offset " ++ show off ++ "."

wordSenseError cat off base = error $
  "Error: Cannot find word sense for " ++ show cat ++ 
  " \"" ++ base ++ "\" at offset " ++ show off ++ "." 

wordError cat off base = error $
  "Error: \"" ++ base ++ "\" is not a member of the " ++ show cat ++
  " synset at offset " ++ show off ++ "."

-- Next for the types that we'll show to the outside world.
data Category = Noun | Verb | Adjective | Adverb
  deriving (Eq,Ord,Show)

data Relation = Identity
  | Synonym      | Antonym
  | Hypernym     | Hyponym
  | InstHypernym | InstHyponym
  | MembHolonym  | MembMeronym
  | PartHolonym  | PartMeronym
  | SubsHolonym  | SubsMeronym
  | TopicDomain  | TopicMember
  | RegionDomain | RegionMember
  | UsageDomain  | UsageMember
  | Head         | Satellite
  | Attribute    | DerivRelated | SeeAlso
  -- Verb, adjective, or adverb specific:
  | Entailment   | Cause        | VerbGroup
  | Participle   | Pertainym    | DerivFrom
  deriving (Eq,Ord,Show)

data WordSense  = WordSense
  { wwCat       :: Category
  , wwBase      :: String
  , wwForm      :: String
  , wwSynset    :: Synset
  , wwRank      :: Int
  , wwRelations :: M.Map Int [Relation]
  } deriving (Eq,Ord,Show)

data Synset = Synset
  { wsCat        :: Category
  , wsWordSenses :: [Int]
  , wsGloss      :: String
  } deriving (Eq,Ord,Show)

data Dictionary = Dictionary
  { wdSynsets    :: [Synset]
  , wdWordSenses :: Array Int WordSense
  , wdWords      :: M.Map (Category,String) [Int]
  } deriving (Eq,Ord,Show)

-- Some utility functions for use during parsing and building matrices.
catFromChar :: Char -> (Category,Bool)
catFromChar 'n' = (Noun,True)
catFromChar 'v' = (Verb,True)
catFromChar 'r' = (Adverb,True)
catFromChar 'a' = (Adjective,True)
catFromChar 's' = (Adjective,False)

catName :: Category -> String
catName Noun      = "noun"
catName Verb      = "verb"
catName Adverb    = "adv"
catName Adjective = "adj"

reflect :: Relation -> Maybe Relation
-- The symmetric ones:
reflect Synonym      = Just Synonym
reflect Antonym      = Just Antonym
reflect Attribute    = Just Attribute
reflect DerivRelated = Just DerivRelated
-- The opposite pairs:
reflect Hypernym     = Just Hyponym
reflect Hyponym      = Just Hypernym
reflect InstHypernym = Just InstHyponym
reflect InstHyponym  = Just InstHypernym
reflect MembHolonym  = Just MembMeronym
reflect MembMeronym  = Just MembHolonym
reflect PartHolonym  = Just PartMeronym
reflect PartMeronym  = Just PartHolonym
reflect SubsHolonym  = Just SubsMeronym
reflect SubsMeronym  = Just SubsHolonym
reflect TopicDomain  = Just TopicMember
reflect TopicMember  = Just TopicDomain
reflect RegionDomain = Just RegionMember
reflect RegionMember = Just RegionDomain
reflect UsageDomain  = Just UsageMember
reflect UsageMember  = Just UsageDomain
reflect Head         = Just Satellite
reflect Satellite    = Just Head
-- The others:
reflect _            = Nothing

relFromCode :: Category -> Bool -> String -> Relation
relFromCode _ _ "!" = Antonym
relFromCode _ _ "@"  = Hypernym
relFromCode _ _ "@i" = InstHypernym
relFromCode _ _ "~"  = Hyponym
relFromCode _ _ "~i" = InstHyponym
relFromCode _ _ "#m" = MembHolonym
relFromCode _ _ "#s" = SubsHolonym
relFromCode _ _ "#p" = PartHolonym
relFromCode _ _ "%m" = MembMeronym
relFromCode _ _ "%s" = SubsMeronym
relFromCode _ _ "%p" = PartMeronym
relFromCode _ _ ";c" = TopicDomain
relFromCode _ _ ";r" = RegionDomain
relFromCode _ _ ";u" = UsageDomain
relFromCode _ _ "-c" = TopicMember
relFromCode _ _ "-r" = RegionMember
relFromCode _ _ "-u" = UsageMember
relFromCode _ _ "="  = Attribute
relFromCode _ _ "+"  = DerivRelated
relFromCode _ _ "^"  = SeeAlso
relFromCode _ _ "*"  = Entailment
relFromCode _ _ ">"  = Cause
relFromCode _ _ "$"  = VerbGroup
relFromCode _ _ "<"  = Participle
relFromCode _ True      "&"  = Satellite
relFromCode _ False     "&"  = Head
relFromCode Adjective _ "\\" = Pertainym
relFromCode Adverb    _ "\\" = DerivFrom
relFromCode c h code = relationError code

-- Intermediate types for use while parsing WordNet data files.
type SynsetId = (Category,Int)
type WordId   = (Category,String)

data IndexEntry = IE
  { wieWordId   :: WordId
  , wieSynsets  :: [Int]
  } deriving (Show)

instance Eq IndexEntry where
  (IE x _) == (IE y _) = x == y

instance Ord IndexEntry where
  compare (IE x _) (IE y _) = compare x y

data DataEntry  = DE
  { wdeSynsetId :: SynsetId
  , wdeIsHead   :: Bool
  , wdeWords    :: [String]
  , wdePointers :: [Pointer]
  , wdeGloss    :: String
  } deriving (Show)

instance Eq DataEntry where
  x == y = wdeSynsetId x == wdeSynsetId y

instance Ord DataEntry where
  compare x y = compare (wdeSynsetId x) (wdeSynsetId y)

data Pointer    = LexPointer
  { wpRelation :: Relation
  , wpSynsetId :: SynsetId
  , wpSourceId :: Int
  , wpTargetId :: Int
  }             | SemPointer
  { wpRelation :: Relation
  , wpSynsetId :: SynsetId
  } deriving (Eq,Ord,Show)

isSemantic :: Pointer -> Bool
isSemantic (SemPointer _ _) = True
isSemantic _                = False

-- Basic parser pieces for the WordNet data files.
chrField   = liftM2 const anyChar space
strField   = liftM2 const (many $ noneOf " ") space
digField   = liftM2 ((read .) . const) (many1 digit) space
fstField   = liftM2 const strField strField
ntrField   = liftM trim $ many $ noneOf "\n"
               where
                 trim = reverse . dropWhile isSpace . reverse
hexField n = liftM2 (((convertHex n) .) . const) (count (2*n) hexDigit) space
               where
                 convertHex 1 = (,) 0 . fst . head . readHex
                 convertHex 2 = flip divMod 256 . fst . head . readHex

-- And the file-level parsers.
wniFile   = endBy wniLine newline
wndFile   = endBy wndLine newline
wniLine   = many wnComment >> wniEntry
wndLine   = many wnComment >> wndEntry
wnComment = string "  " >> many (noneOf "\n") >> newline

-- And the line-level stuff.
wniEntry = do
  lemma   <- strField
  (cat,_) <- liftM catFromChar chrField
  synCt   <- digField
  ptrCt   <- digField
  count ptrCt strField >> strField >> strField
  syns    <- count synCt digField
  many $ noneOf "\n"
  return $ IE (cat,lemma) syns

wndEntry = do
  offset     <- digField
  strField
  (cat,h)    <- liftM catFromChar chrField
  (_,wordCt) <- hexField 1
  words      <- count wordCt fstField
  ptrCt      <- digField 
  pointers   <- count ptrCt (wndPointer cat h)
  many (noneOf "|") >> char '|' >> spaces
  gloss      <- ntrField
  return $ DE (cat,offset) h words pointers gloss

wndPointer cat isHead = do
  rel     <- liftM (relFromCode cat isHead) strField
  offset  <- digField
  (cat,_) <- liftM catFromChar chrField
  (s,t)   <- hexField 2
  return $ if (s,t) == (0,0)
             then SemPointer rel (cat,offset)
             else LexPointer rel (cat,offset) s t

-- A couple of utility functions for parsing.
parseFile parser base name cat = do
  c <- readFile (base ++ name ++ catName cat)
  return $ parse parser "" c

parseFiles base = do
  nInd <- parseFile wniFile base "index." Noun
  vInd <- parseFile wniFile base "index." Verb
  aInd <- parseFile wniFile base "index." Adjective
  rInd <- parseFile wniFile base "index." Adverb

  nDat <- parseFile wndFile base "data." Noun
  vDat <- parseFile wndFile base "data." Verb
  aDat <- parseFile wndFile base "data." Adjective
  rDat <- parseFile wndFile base "data." Adverb

  ind <- case nInd <++> vInd <++> rInd <++> aInd of
    Left e  -> do print e; return []
    Right r -> return r

  dat <- case nDat <++> vDat <++> rDat <++> aDat of
    Left e  -> do print e; return []
    Right r -> return r

  return (sort ind,sort dat)
  
buildDict ind dat = Dictionary (map fst $ M.elems synsetMap) (listArray (1,length wordSenses) wordSenses) wordMap
  where
    formToBase = takeWhile (/='(') . map toLower

    explodeIndices :: [IndexEntry] -> [((Category,String),Int)]
    explodeIndices ind = extract =<< ind
      where
        extract (IE wordId offs) = zip (repeat wordId) offs

    expInd = explodeIndices ind

    arr :: Array Int (Category,String)
    arr = listArray (1,length expInd) $ map extract expInd
      where
        extract ((cat,base),off) = (cat,base)

    wordMap :: M.Map WordId [Int]
    wordMap = M.fromAscListWith (++) $ zip (map fst expInd) $ map return [1..]

    synMap :: M.Map SynsetId [Int]
    synMap = M.fromListWith (++) $ zip (map (\((c,_),o) -> (c,o)) expInd) $ map return [1..]

    synsetMap :: M.Map SynsetId (Synset,[String])
    synsetMap = M.fromAscList $ map extract dat
      where
        extract (DE synId@(cat,off) _ ws _ gloss) = (synId,((Synset cat wis gloss),ws))
          where
            ids = synMap M.! synId
            wds = M.fromList $ zip (map (snd . (arr !)) ids) ids
            wis = map ((wds M.!) . formToBase) ws

    rels = M.fromListWith (++) $ extract =<< dat
      where
        extract (DE x _ _ pointers _) = reflected $ extract' =<< pointers
          where
            lookupWordSenses x = wsWordSenses $ fst $ synsetMap M.! x
            extract' (SemPointer rel y) = [(xi,[(yi,[rel])])|xi <- xis,yi <- yis,xi /= yi]
              where
                xis = lookupWordSenses x 
                yis = lookupWordSenses y
            extract' (LexPointer rel y s t) = [(xi,[(yi,[rel])])]
              where
                xi = lookupWordSenses x !! (s-1)
                yi = lookupWordSenses y !! (t-1)
            reflected xs = xs ++ (reflected' =<< xs)
              where
                reflected' (xi,((yi,(rel:[])):[])) = maybe [] (return . (,) yi . return . (,) xi . return) $ reflect rel

    wordSenses :: [WordSense]
    wordSenses = map extract $ zip [1..] expInd
      where
        extract (i,((cat,base),off)) = WordSense cat base (forms!!rank) synset rank $ M.map (nub . sort) $ M.fromListWith (++) relations
          where
            rank = maybe (error "Problem.") id $ elemIndex i $ wsWordSenses synset
            synId = (cat,off)
            (synset,forms) = synsetMap M.! synId
            relations = (i,[Identity]) : zip (delete i $ nub $ wsWordSenses synset) (repeat [Synonym]) ++
              (maybe [] id $ i `M.lookup` rels)

