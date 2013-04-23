
module My.Utils where

import XMonad.Util.Run (runProcessWithInput)

import Control.Applicative ((<$>))
import Data.Char
import Data.List  (isPrefixOf,intercalate,find)
import Data.Maybe (fromMaybe)
import Data.Foldable (foldlM)

monitorResolution :: String -> IO (Maybe Int)
monitorResolution mon =
  fmap (read . keepUntil (== 'x') . dropWhile isSpace) .
  find (elem '*') .
  underMonitor <$>
  pipe [ ("xrandr",[]) ] ""
  where
  underMonitor s =
    let ls' = dropWhile (not . isPrefixOf mon) (lines s) in
      head ls' : keepUntil (not . isPrefixOf " ") (tail ls')

keepUntil :: (a -> Bool) -> [a] -> [a]
keepUntil p as = case as of
  []    -> []
  a:as' -> if p a
    then []
    else a : keepUntil p as'

cmdArgs :: String -> [(String,String)] -> String
cmdArgs cmd args = unwords cmd :
  concatMap (\(flag,arg) -> if null arg then [flag] else [flag,arg]) args

qt :: String -> String
qt s = "'" ++ s ++ "'"

pipe :: [(String,[String])] -> String -> IO String
pipe = flip $ foldlM $ \input (cmd,args) -> runProcessWithInput cmd args input

branch :: a -> a -> Bool -> a
branch t f b = if b then t else f

