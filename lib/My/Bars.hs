
module My.Bars
  ( statusBarMain
  , statusBarExternal
  ) where

import XMonad.Util.Run

import System.IO (Handle)
import Control.Arrow (second)
import Control.Monad (when)

import My.Decor

mySBarsStart =                                   0
mySBars      = [ (dzenBar "l"               ,  800) ]

myBarsStart  =                                 800
myBars       = [ (conkyBar ".conkytime" "c" ,  1120)
               , (conkyBar ".conkytop"  "r" , 1840)
               , (stalonetray               , 1920)
               ]

sbarsMain = bars mySBarsStart mySBars
barsMain  = bars myBarsStart myBars

sbarsExternal = bars (mySBarsStart+1280) $ map (second (+1280)) mySBars
barsExternal  = bars (myBarsStart+1280) $ map (second (+1280)) myBars

---------------------------------------------------------------------

statusBarMain :: Bool -> IO [Handle]
statusBarMain  = statusBar sbarsMain barsMain

statusBarExternal :: Bool -> IO [Handle]
statusBarExternal  = statusBar sbarsExternal barsExternal

statusBar sts cs shouldRun = do
  when shouldRun $ mapM_ spawnPipe cs
  if shouldRun
    then mapM spawnPipe sts
    else return []

conkyBar f a x0 x1 = concat
  [ "conky -c "
  , f
  , " | "
  , dzenBar a x0 x1
  ]

dzenBar a x0 x1 = concat
  [ "dzen2 -x '"
  , show x0
  , "' -y '0' -h '16' -w '"
  , show (x1-x0)
  , "' -ta '"
  , a
  , "' -fg '"
  , fg normal
  , "' -bg '"
  , bg normal
  , "' -fn '"
  , myFont
  , "'"
  ]

stalonetray x0 x1 = concat
  [ "stalonetray --geometry '"
  , show w
  , "x1+"
  , show x0
  , "+0' -i '16' -bg '"
  , bg normal
  , "' --icon-gravity NE"
  , " --kludges=force_icons_size"
  ]
  where w = (x1 - x0) `div` 16

bars :: Int -> [((Int -> Int -> String),Int)] -> [String]
bars x bs = case bs of
  []            -> []
  ((f,x'):rest) -> f x x' : bars x' rest

