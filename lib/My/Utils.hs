
module My.Utils where

import XMonad.Util.Run (runProcessWithInput)

import Control.Applicative ((<$>))
import Data.List  (isPrefixOf,intercalate)
import Data.Maybe (fromMaybe)

connectedToExt :: String -> IO Bool
connectedToExt mon = any (elem '*') <$> under <$> lines <$> pipe [ ("xrandr",[]) ] ""
  where
    under ls =
      let ls' = dropWhile (not . isPrefixOf mon) ls in
        head ls' : (keepUntil (not . isPrefixOf " ") $ tail ls')

keepUntil :: (a -> Bool) -> [a] -> [a]
keepUntil p as = case as of
  [] -> []
  a:as'
    | p a ->       []
    | otherwise -> a : keepUntil p as'

cmdArgs :: String -> [(String,String)] -> String
cmdArgs cmd args = intercalate " " $ cmd :
  concatMap (\(flag,arg) -> if null arg then [flag] else [flag,arg]) args

qt :: String -> String
qt s = "'" ++ s ++ "'"

pipe :: [(String,[String])] -> String -> IO String
pipe [] res              = return res
pipe ((c,args):rest) res = runProcessWithInput c args res >>= pipe rest

branch :: a -> a -> Bool -> a
branch t f b = if b then t else f

