module Sentlex where

import Control.Arrow
import Control.Monad (liftM)
import Data.Array
import Data.Array.ST
import Data.List (delete,groupBy,nub,sort,sortBy)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec

import WordNet
import qualified Liwc as L

relevantSeedsToV :: Dictionary -> (Relation -> Double) -> Int -> [((Category,String),Double)] -> Array Int Double
relevantSeedsToV d s it seeds = accumArray (+) 0.0 bnds $ concat $ map (\(a,b) -> map (\((c,_),_) -> (c,b)) a) rels  -- $ map (map (\(((a,_),v),_) -> (a,v)) . {-relevant .-} 
  where
    rels = map (relevant . (uncurry $ rateSenses d s it)) $ withComp seeds
    bnds = bounds $ wdWordSenses d
    withComp xs = map (\x -> (x,delete x xs)) xs

seedsToV :: Dictionary -> [((Category,String),Double)] -> Array Int Double
seedsToV d seeds = accumArray (+) 0.0 bnds $ concat $ map lookupSeed seeds
  where
    bnds = bounds $ wdWordSenses d
    lookupSeed (seed,w) = zip ids $ repeat w
      where
       ids = case M.lookup seed $ wdWords d of
         Just i  -> i
         Nothing -> error $ "There was an error in the seed file for word: " ++ show seed ++ "."

rateSenses :: Dictionary -> (Relation -> Double) -> Int -> ((Category,String),Double) -> [((Category,String),Double)] -> ([((Int,WordSense),Double)],Double)
rateSenses d s it (w,wv) os = (map f ids,wv)
  where
    f i = ((i,(wdWordSenses d) ! i),v ! i)
    v = last $ take it $ iterate (multiply d s) $ seedsToV d $ os
    ids = case M.lookup w $ wdWords d of
      Just i  -> i
      Nothing -> error $ "There was an error in the seed list for word: " ++ show w ++ "."

relevant :: ([(a,Double)],Double) -> ([(a,Double)],Double)
relevant (ws,w) | all ((==0.0) . snd) ws = (ws,w)
                | w > 0.0                = (filter ((>=(cutoff $ sortSnd ws)) . snd) ws,w)
                | otherwise              = (filter ((>=(cutoff $ sortSnd $ map (\(a,b) -> (a,-1.0*b)) ws)) . snd) ws,w)
  where
    eps = 0.0000001
    sortSnd = sortBy (\(_,a) (_,b) -> compare a b)
    avg vs = sum vs / (fromIntegral $ length vs)

    cutoff ws = cutoff' $ (snd $ last ws) / 2.0
      where
        cutoff' oc | abs (nc - oc) > eps = cutoff' nc
                   | otherwise           = nc
          where
            nc = (avg $ map snd $ filter ((>=oc) . snd) ws) / 2.0


multiply :: (Eq a, Fractional a) => Dictionary -> (Relation -> a) -> Array Int a -> Array Int a
multiply d s v = accumArray (+) 0.0 (bounds v) $ expand v
  where
    nonzero (i,e) = e /= 0.0
    expand v = concat $ map rels $ filter ((/=0.0) . snd) $ assocs v
    rels (i,e) = map (second $ (e*) . sum . map s) $ M.toList $ wwRelations $ (wdWordSenses d) ! i

seedFile = endBy seedLine newline
seedLine = do
  polarity <- liftM (\p -> if p == '+' then 1.0 else -1.0) $ oneOf "+-"
  spaces   >> oneOf "," >> spaces
  lemma    <- many $ noneOf ","
  spaces   >> oneOf "," >> spaces
  (cat,_)  <- liftM catFromChar $ oneOf "nvra"
  return ((cat,lemma),polarity)

parseSeedFile :: FilePath -> IO [((Category,String),Double)]
parseSeedFile name = do
  c <- readFile name
  return $ case parse seedFile "" c of
    Right s -> s
    Left  e -> error $ "There was an error loading the seed file: " ++ show e

