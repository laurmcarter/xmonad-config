
module My.Bars
  ( statusBarMain
  , statusBarExternal
  ) where

import XMonad.Util.Run

import My.Utils

import System.IO (Handle)
import Control.Arrow (second)
import Control.Monad (when)

import My.Decor

mon0x = 1920
mon1x = 1280

mySBarsStart w =                                 0
mySBars      w = [ (dzenBar "l"               ,  myBarsStart w) ]

myBarsStart  w =                                half w - 200
myBars       w = [ (conkyBar ".conkytime" "c" , half w + 200)
                 , (conkyBar ".conkytop"  "r" , w - 80)
                 , (stalonetray               , w)
                 ]

half = (`div` 2)

sbarsMain = bars (mySBarsStart mon0x) (mySBars mon0x)
barsMain  = bars (myBarsStart mon0x) (myBars mon0x)

sbarsExternal = bars (mySBarsStart mon1x + mon0x) (map (second (+ mon0x)) (mySBars mon1x))
barsExternal  = bars (myBarsStart mon1x + mon0x) (map (second (+ mon0x)) (myBars mon1x))

---------------------------------------------------------------------

statusBarMain :: Bool -> IO [Handle]
statusBarMain  = statusBar sbarsMain barsMain

statusBarExternal :: Bool -> IO [Handle]
statusBarExternal  = statusBar sbarsExternal barsExternal

statusBar :: [String] -> [String] -> Bool -> IO [Handle]
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

dzenBar a x0 x1 = cmdArgs "dzen2"
  [ ( "-x"  , qt $ show x0 )
  , ( "-y"  , qt $ "0" )
  , ( "-h"  , qt $ "16" )
  , ( "-w"  , qt $ show (x1-x0) )
  , ( "-ta" , qt $ a )
  , ( "-fg" , qt $ fg normal )
  , ( "-bg" , qt $ bg normal )
  , ( "-fn" , myFont 12 )
  , ( "-e"  , "'button3=ungrabkeys'" )
  ]

stalonetray x0 x1 = cmdArgs "stalonetray"
  [ ( "--geometry"     , qt $ concat [ show w , "x1+" , show x0 , "+0" ] )
  , ( "-i"             , qt $ "16" )
  , ( "-bg"            , qt $ bg normal )
  , ( "--icon-gravity" , qt $ "NE" )
  ]
  where w = (x1 - x0) `div` 16

bars :: Int -> [((Int -> Int -> String),Int)] -> [String]
bars x bs = case bs of
  []            -> []
  ((f,x'):rest) -> f x x' : bars x' rest

