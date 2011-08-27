module Liwc (parseFile,lookup) where

import Prelude hiding (lookup)
import Control.Monad (mplus)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec

-- This is a kind of search tree where the first data item represents a value
-- for an exact match on this node, the second for a starred match, and the
-- last for the children.
data Tree a = Tree (Maybe a) (Maybe a) (M.Map Char (Tree a))

-- Functions for creating new trees.
blank = Tree Nothing Nothing
empty = Tree Nothing Nothing M.empty

-- Inserting a new entry.
insert :: (Show a) => String -> a -> Tree a -> Tree a
insert ('*':[]) v (Tree t _ m) = Tree t (Just v) m
insert []       v (Tree _ s m) = Tree (Just v) s m
insert (k:ks)   v (Tree t s m) = Tree t s $ M.alter (Just . f) k m
                                 where
                                   f = insert ks v . fromMaybe empty

-- Looking up an entry.
lookup [] (Tree t s _)        = mplus t s --maybe s Just t
lookup k  (Tree _ (Just s) m) = Just $ fromMaybe s $ lookup k $ blank m
lookup (k:ks) (Tree _ _ m)    = M.lookup k m >>= lookup ks

-- Parse the JSON-format LIWC lexicon file.
parseFile file = do
  c <- fmap (either (const []) id . parse lexicon "") $ readFile file
  return $ foldr (uncurry insert) empty $ concat c

-- Our parser combinators.
normal = many $ noneOf "\","

lexicon = do
  spaces >> char '{'
  subhashes <- sepBy subhash $ char ','
  spaces >> char '}' >> spaces
  return subhashes

subhash = do
  spaces >> char '"' >> normal >> char '"'
  spaces >> char ':' >> spaces >> char '{'
  entries <- sepBy entry $ char ','
  char '}' >> spaces
  return entries

entry = do
  spaces >> char '"'
  key <- normal
  char '"' >> spaces >> char ':' >> spaces >> char '"'
  cats <- sepBy normal $ char ','
  char '"' >> spaces
  return (key,cats)

