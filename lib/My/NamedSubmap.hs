
module My.NamedSubmap where

import XMonad
import XMonad.Actions.Submap
import XMonad.Util.EZConfig

import My.Utils

import Control.Arrow
import qualified Data.Map as M
import System.Process
import System.IO
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

-- | Self-advertising submap that pops up a temporary, roll-open dzen window describing the
--   available keybindings.
namedSM :: DzenConfig -> XConfig a -> Int -> Int -> String -> [(String,String,X())] -> X ()
namedSM dzc xc x0 y0 title km = do
  pid <- io $ do
    (i,o,e,pid) <- runInteractiveCommand $ dzenBar dzc (length km) y0 x0 (x0 + boxWidth)
    hPutStrLn i ("^pa(10)"++title)
    mapM_ (hPutStrLn i . alignDzen dzc maxKeyLen) km
    hFlush i
    return pid
  submap $ mkKeymap xc $ reduceKeys km
  io $ terminateProcess pid
  where
  boxWidth = round (fontWidth dzc * fromIntegral maxLineLen) + gap dzc + 10
  numLines = length km
  maxLineLen = maximum $ length title : map (\(k,t,_)->1 + length k + length t) km
  maxKeyLen = maximum $ map (\(k,_,_)->length k) km



-- | Named Submap that reenters itself upon taking an action.
--   Any keypress not in the submap exits the map.
--   An exit key binding may be explicitly provided.
modeSM :: DzenConfig -> XConfig a -> Maybe String -> Int -> Int -> String -> [(String,String,X ())] -> X ()
modeSM smc xc mexit x0 y0 title km = namedSM smc xc x0 y0 title $ addExit mexit km'
  where
  km' = map loopBack km
  loopBack (k,n,x) = (k,n,x >> namedSM smc xc x0 y0 title km')
  addExit = maybe id $ \k -> ((k,"Exit Mode", return ()) :)



-- | Mode Submap that only has two actions, an Up and Down.
upDownModeSM :: DzenConfig -> XConfig a -> Maybe String -> Int -> Int -> String -> (String,X ()) -> (String,X ()) -> X ()
upDownModeSM smc xc mexit x0 y0 title (upK,upC) (downK,downC) = modeSM smc xc mexit x0 y0
  (unwords ["-",title,"+"])
  [ (upK,"Up",upC), (downK,"Down",downC) ]



-- Helpers ---------------------------------------------------------------------

alignDzen :: DzenConfig -> Int -> (String, String, X ()) -> String
alignDzen smc len (k,t,_) = concat
  [ "^pa(10)" 
  , k
  , "^pa("
  , show (round (fontWidth smc * fromIntegral len) + gap smc)
  , ")"
  , t
  ]

-- | Takes a named keymap list and reduces it to a normal associative list.
reduceKeys :: [(a,b,c)] -> [(a,c)]
reduceKeys = map (\(x,_,y)->(x,y))

