module Main where

import Control.Monad (liftM)
import Data.Array
import Data.List (groupBy,nub,sort,sortBy)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import System (getArgs)
import Text.ParserCombinators.Parsec
import Text.Printf
import Sentlex
import WordNet
import qualified Liwc as L

seedsToV bnds words seeds = accumArray (+) 0 bnds $ concat $ map findSeed seeds
  where
    findSeed (word,weight) = zip wordIds $ repeat weight
      where
       wordIds = case M.lookup word words of
         Just i -> i
         Nothing -> error $ "There was an error in the seed file for word: " ++ show word

baseWeight = 0.2
weight Identity     = baseWeight + 1.0
weight Antonym      = -baseWeight
weight Synonym      = baseWeight
weight Hypernym     = baseWeight / 3.0
--weight Hyponym      = baseWeight / 3.0
weight Head         = baseWeight / 1.0
weight Satellite    = baseWeight / 1.0
weight TopicDomain  = baseWeight / 5.0
weight TopicMember  = baseWeight / 5.0
weight Participle   = baseWeight / 2.0
weight DerivRelated = baseWeight / 2.0
weight _ = 0.0

expando wordSenses v = sortBy (\(_,a,_) (_,b,_) -> compare b a) $ map rearr $ groupBy (\a b -> fst a == fst b) $ sort $ map expand $ filter (\(i,e) -> (abs e) >= 0.0) $ assocs v
  where
    avg rs = (sum $ map snd rs) / (fromIntegral $ length rs) -- :: (Fractional c) => [(a,b)] -> c
    rearr rs = (fst $ head rs, avg rs, map snd rs)
    expand (i,e) = ((wwForm $ wordSenses ! i,wwCat $ wordSenses ! i),e)

clean :: ((String,Category),Double,[Double]) -> String
clean ((w,_),a,_) = printf "%20s %0.4f\n" w a

main = do
  {-(liwcFile:w:_) <- getArgs
  l <- L.parseFile liwcFile
  print $ L.lookup w l-}

  {-
  (wnBase:seedFile:_) <- getArgs
  (ind,dat) <- parseFiles wnBase
 
  let d0 = buildDict ind dat
  let d1 = buildDict' ind dat

  let a0 = map wwRelations $ elems $ wdWordSenses d0
  let a1 = map wwRelations $ elems $ wdWordSenses d1

  mapM_ print $ map (\(a,b) -> show a ++ "~" ++ show b) $ zip a0 a1 --map wsCat $ wdSynsets d0) (map wsCat $ wdSynsets d1)
  -}
 
  --{-
  
  (wnBase:seedFile:_) <- getArgs
  (ind,dat) <- parseFiles wnBase
  seeds <- parseSeedFile seedFile

  let dict = buildDict ind dat
  let v0 = relevantSeedsToV dict weight 5 seeds
  let v = last $ take 5 $ iterate (multiply dict weight) v0
  mapM_ (putStr . clean) $ filter (\(_,a,_) -> a /= 0.0)  $ expando (wdWordSenses dict) v
  
  --}

  --mapM_ putStr $ map f $ relevant $ rateSenses dict weight 20 ((Noun,"lizard"),1.0) [((Adjective,"great"),1.0),((Adjective,"best"),1.0),((Adjective,"fine"),1.0),((Adjective,"happy"),1.0),((Verb,"like"),1.0)]
  --  where f (((_,w),_),d) = show d ++ ": " ++ (wwForm w) ++ " " ++ wsGloss (wwSynset w) ++ "\n"

  {-case parse lexicon "" c of
    Left a  -> []
    Right a -> a-}
  
  {-(wnBase:seedFile:_) <- getArgs
  (ind,dat) <- parseFiles wnBase
  seeds <- parseSeedFile seedFile

  let dict = buildDict ind dat
  --mapM_ print $ fromMaybe [] $ M.lookup (Noun,"line") (wdWords dict)
  let v0 = seedsToV (bounds $ wdWordSenses dict) (wdWords dict) seeds
  --mapM_ print $ (M.toList . wdWords) dict

  --seeds <- let expand ss = case ss of
  --           Right s -> s
  --           Left  e -> error $ "There was an error loading the seed file: " ++ show e
  --         in expand $ parseSeedFile seedFile

  --return 0
  let v1 = multV weight (wdWordSenses dict) v0
  let v2 = multV weight (wdWordSenses dict) v1
  let v3 = multV weight (wdWordSenses dict) v2
  let v4 = multV weight (wdWordSenses dict) v3
  let v5 = multV weight (wdWordSenses dict) v4
--  let v6 = multV weight (wdWordSenses dict) v5
--  let v7 = multV weight (wdWordSenses dict) v6
--  let v8 = multV weight (wdWordSenses dict) v7
--  let v9 = multV weight (wdWordSenses dict) v8
--  let vx = multV weight (wdWordSenses dict) v9
--  mapM_ (putStr . clean) $ expando (wdWordSenses dict) v5
  mapM_ print $ filter (\(_,a,_) -> a /= 0.0) $ expando (wdWordSenses dict) v5
-}


  --mapM_ print $ M.toList (wdWords dict)

  --print $ length $ buildDict ind dat
  --mapM_ putStr $ map f $ buildDict ind dat
  --  where f (WordSense cat base form syn rank rels) = (show cat) ++ " " ++ base ++ " " ++ form ++ " " ++ (show rank) ++ (show $ M.toList rels) ++ "\n"

