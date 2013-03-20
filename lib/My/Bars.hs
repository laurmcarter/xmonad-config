
module My.Bars
  ( statusBarMain
  , statusBarExternal
  ) where

import XMonad.Util.Run

import System.IO (Handle)
import Control.Arrow (second)
import Control.Applicative
import Control.Monad (when)

import My.Utils
import My.Decor

mySBarsStart w =                                 0
mySBars      w = [ (dzenBar "l"               ,  myBarsStart w) ]

myBarsStart  w =                                half w - 200
myBars       w = [ (conkyBar ".conkytime" "c" , half w + 200)
                 , (conkyBar ".conkytop"  "r" , w - 80)
                 , (stalonetray               , w)
                 ]

half = (`div` 2)

sbarsMain x = bars (mySBarsStart x) (mySBars x)
barsMain  x = bars (myBarsStart x) (myBars x)

sbarsExternal (x0,x1) = bars (mySBarsStart x1 + x0) (map (second (+ x0)) (mySBars x1))
barsExternal (x0,x1) = bars (myBarsStart x1 + x0) (map (second (+ x0)) (myBars x1))

---------------------------------------------------------------------

statusBarMain :: Maybe Int -> IO [Handle]
statusBarMain x = statusBar ((,) <$> (sbarsMain <$> x) <*> (barsMain <$> x))

statusBarExternal :: Maybe (Int,Int) -> IO [Handle]
statusBarExternal xs = statusBar ((,) <$> (sbarsExternal <$> xs) <*> (barsExternal <$> xs))

statusBar :: Maybe ([String],[String]) -> IO [Handle]
statusBar m = case m of
  Just (sts,cs) -> do
    mapM_ spawnPipe cs
    mapM spawnPipe sts
  Nothing       -> return []

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

