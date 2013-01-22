
module My.NamedSubmap
  ( SMConfig(..)
  , defaultSMConfig
  , namedSM
  , modeSM
  , reduceKeys
  ) where

import XMonad
import XMonad.Actions.Submap
import XMonad.Util.EZConfig

import Control.Arrow
import qualified Data.Map as M
import System.Process
import System.IO
import Data.List (intercalate)

data SMConfig = SMConfig
  { xPos      :: Int
  , yPos      :: Int
  , gap       :: Int
  , dzenFont  :: Maybe String
  , fontWidth :: Rational
  , fgDzen    :: String
  , bgDzen    :: String
  }

defaultSMConfig :: SMConfig
defaultSMConfig = SMConfig
 { xPos = 20
 , yPos = 20
 , gap = 20
 , dzenFont  = Nothing
 , fontWidth = 11.5
 , fgDzen = "#ffffff"
 , bgDzen = "#808080"
 }

modeSM :: SMConfig -> XConfig a -> Maybe String -> String -> [(String,String,X ())] -> X ()
modeSM smc xc mexit title km = 
  namedSM smc xc title $
    maybe id (\k -> ((k,"Exit Mode",return ()) :)) mexit $ km'
  where
  km' = map (\(k,n,x) -> (k,n,x >> namedSM smc xc title km')) km

namedSM :: SMConfig -> XConfig a -> String -> [(String,String,X())] -> X ()
namedSM smc xc title km = do
  pid <- io $ do
    (i,o,e,pid) <- runInteractiveCommand (intercalate " " $
      ([ "dzen2"
       , "-fg" , "'" ++ fgDzen smc ++ "'"
       , "-bg" , "'" ++ bgDzen smc ++ "'"
       , "-x"  , show $ xPos smc
       , "-y"  , show $ yPos smc
       , "-ta" , "l"
       , "-sa" , "l"
       , "-l"  , show $ length km
       , "-w"  , show ((round (fontWidth smc * fromIntegral maxLineLen)) + gap smc + 10)
       ] ++ (maybe [] (\f->["-fn",f]) $ dzenFont smc)
       ))
    hPutStrLn i ("^pa(10)"++title)
    mapM (hPutStrLn i . alignDzen smc maxKeyLen) km
    hFlush i
    return pid
  submap $ mkKeymap xc $ reduceKeys km
  io $ terminateProcess pid
  where
    numLines = length km
    maxLineLen = maximum $ length title : map (\(k,t,_)->1 + length k + length t) km
    maxKeyLen = maximum $ map (\(k,_,_)->length k) km

alignDzen :: SMConfig -> Int -> (String, String, X ()) -> String
alignDzen smc len (k,t,_) = "^pa(10)" ++ k ++ "^pa("++show ((round (fontWidth smc * fromIntegral len)) + gap smc)++")"++t

reduceKeys :: [(a,b,c)] -> [(a,c)]
reduceKeys = map (\(x,_,y)->(x,y))

