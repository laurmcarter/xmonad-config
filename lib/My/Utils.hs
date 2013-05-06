
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
cmdArgs cmd args = unwords $ cmd :
  concatMap (\(flag,arg) -> if null arg then [flag] else [flag,arg]) args

qt :: String -> String
qt s = "'" ++ s ++ "'"

pipe :: [(String,[String])] -> String -> IO String
pipe = flip $ foldlM $ \input (cmd,args) -> runProcessWithInput cmd args input

branch :: a -> a -> Bool -> a
branch t f b = if b then t else f

-- | Configuration for popup Dzen.
data DzenConfig = DzenConfig
  { gap        :: Int
  , lineHeight :: Int
  , dzenFont   :: Maybe String
  , fontWidth  :: Rational
  , dzenFg     :: String
  , dzenBg     :: String
  , titleAlign :: DzenAlign
  , slaveAlign :: DzenAlign
  }

defaultDzenConfig :: DzenConfig
defaultDzenConfig = DzenConfig
 { gap = 20
 , lineHeight = 16
 , dzenFont  = Nothing
 , fontWidth = 11.5
 , dzenFg = "#ffffff"
 , dzenBg = "#808080"
 , titleAlign = LeftAlign
 , slaveAlign = LeftAlign
 }

data DzenAlign
  = LeftAlign
  | CenterAlign
  | RightAlign

instance Show DzenAlign where
  show a = case a of
    LeftAlign -> "l"
    CenterAlign -> "c"
    RightAlign -> "r"

dzenBar :: DzenConfig -> Int -> Int -> Int -> String
dzenBar dzc y0 x0 x1 = cmdArgs "dzen2" $
  [ ( "-x"  , qt $ show x0 )
  , ( "-y"  , qt $ show y0 )
  , ( "-h"  , qt $ show $ lineHeight dzc )
  , ( "-w"  , qt $ show (x1 - x0) )
  , ( "-ta" , qt $ show $ titleAlign dzc )
  , ( "-sa" , qt $ show $ slaveAlign dzc )
  , ( "-fg" , qt $ dzenFg dzc )
  , ( "-bg" , qt $ dzenBg dzc )
  , ( "-e"  , "'button3=ungrabkeys'" )
  ] ++ [ ( "-fn" , qt fn ) | Just fn <- [dzenFont dzc] ]

