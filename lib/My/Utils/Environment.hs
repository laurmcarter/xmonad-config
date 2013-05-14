module My.Utils.Environment
  ( withCurrentEnv
  )
  where

import System.Environment

import Control.Applicative
import Data.List (break)

data Format
  = L String
  | V String
  deriving Show

parse :: String -> [Format]
parse s = case s of
  ""     -> []

  '%':'%':s' -> foldInLit '%' $ parse s'
  '%':s' -> let (mv,rest) = parseVar s' in case mv of
    Nothing -> parse rest
    Just ""  -> parse rest
    Just v   -> V v : parse rest

  c  :s' -> foldInLit c $ parse s'

foldInLit :: Char -> [Format] -> [Format]
foldInLit c fs = case fs of
  L l : fs' -> L (c:l) : fs'
  _         -> L [c] : fs

parseVar :: String -> (Maybe String,String)
parseVar s = case s of
  ""    -> (Nothing,s)
  '%':_ -> (Nothing,s)
  '(':v -> let (var,rest) = loop v in (Just var,rest)
  where
  loop s = case s of
    "" -> error $ "Format fragment: " ++ show s
    ')':rest -> ("",rest)
    c:s'     -> let (v,rest') = loop s' in (c:v,rest')

withCurrentEnv :: String -> IO String
withCurrentEnv s = concat <$> ss
  where
  ss = sequence $ runFormat $ parse s

runFormat :: [Format] -> [IO String]
runFormat = map $ \f -> case f of
  L s -> return s
  V s -> getEnv s
  

