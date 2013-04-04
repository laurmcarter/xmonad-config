
module My.NamedSubmap
  ( SMConfig(..)
  , defaultSMConfig
  , namedSM
  , modeSM
  , upDownModeSM
  , reduceKeys
  ) where

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

-- | Configuration for popup Dzen.
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

-- | Named Submap that reenters itself upon taking an action.
--   Any keypress not in the submap exits the map.
--   An exit key binding may be explicitly provided.
modeSM :: SMConfig -> XConfig a -> Maybe String -> String -> [(String,String,X ())] -> X ()
modeSM smc xc mexit title km = 
  namedSM smc xc title $
    maybe id (\k -> ((k,"Exit Mode",return ()) :)) mexit $ km'
  where
  km' = map (\(k,n,x) -> (k,n,x >> namedSM smc xc title km')) km

upDownModeSM :: SMConfig -> XConfig a -> Maybe String -> String -> (String,X ()) -> (String,X ()) -> X ()
upDownModeSM smc xc mexit title (upK,upC) (downK,downC) = modeSM smc xc mexit title $
  [ (upK,"Up",upC), (downK,"Down",downC) ]

-- | Self-advertising submap that pops up a temporary, roll-open dzen window describing the
--   available keybindings.
namedSM :: SMConfig -> XConfig a -> String -> [(String,String,X())] -> X ()
namedSM smc xc title km = do
  pid <- io $ do
    (i,o,e,pid) <- runInteractiveCommand $
      cmdArgs "dzen2"
        [ ( "-fg" , qt $ fgDzen smc             )
        , ( "-bg" , qt $ bgDzen smc             )
        , ( "-x"  , show $ xPos smc             )
        , ( "-y"  , show $ yPos smc             )
        , ( "-ta" , "l"                         )
        , ( "-sa" , "l"                         )
        , ( "-l"  , show $ length km            )
        , ( "-w"  , show boxWidth               )
        , ( "-fn" , fromMaybe "" $ dzenFont smc )
        ]
    hPutStrLn i ("^pa(10)"++title)
    mapM (hPutStrLn i . alignDzen smc maxKeyLen) km
    hFlush i
    return pid
  submap $ mkKeymap xc $ reduceKeys km
  io $ terminateProcess pid
  where
    boxWidth = round (fontWidth smc * fromIntegral maxLineLen) + gap smc + 10
    numLines = length km
    maxLineLen = maximum $ length title : map (\(k,t,_)->1 + length k + length t) km
    maxKeyLen = maximum $ map (\(k,_,_)->length k) km

alignDzen :: SMConfig -> Int -> (String, String, X ()) -> String
alignDzen smc len (k,t,_) = "^pa(10)" ++ k ++ "^pa("++show ((round (fontWidth smc * fromIntegral len)) + gap smc)++")"++t

-- | Takes a named keymap list and reduces it to a normal associative list.
reduceKeys :: [(a,b,c)] -> [(a,c)]
reduceKeys = map (\(x,_,y)->(x,y))

