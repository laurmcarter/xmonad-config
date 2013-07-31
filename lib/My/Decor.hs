
module My.Decor where

import XMonad.Prompt
import XMonad.Hooks.DynamicLog
import XMonad.Util.WorkspaceCompare

import My.Utils

import Data.Char
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import System.IO (hPutStrLn)

myBorderWidth :: Word32
myBorderWidth = 1
myFont :: Int -> String
--myFont s       = "-*-terminus-medium-r-normal-*-" ++ show s ++ "-*-*-*-*-*-*-*"
myFont s      = "'Terminus:size=" ++ show s ++ "'"

normal  = CS { fg = "#839496" , bg = "#002b36" }
urgent  = CS { fg = "#ffffff" , bg = "#cb4b16" }
focused = CS { fg = "#f0f0f0" , bg = "#333333" }
borders = CS { fg = "#433ffe" , bg = "#010062" }
light   = CS { fg = "#002b36" , bg = "#839496" }

myDzenPP hs = PP 
  { ppCurrent         = clickWS (dzenColor "#84b000" "" . wrap "[" "]")
  , ppVisible         = clickWS (dzenColor "#657b83" "" . wrap "[" "]")
  , ppHidden          = clickWS (dzenColor "#435459" "" . pad)
  , ppHiddenNoWindows = const ""
  , ppUrgent          = clickWS (dzenColor (fg urgent) (bg urgent) . pad)
  , ppSep             = dzenColor "#268bd2" "" " :: "
  , ppWsSep           = " "
  , ppTitle           = shorten 60
  , ppLayout          = id
                        -- don't care about the layout name.
  , ppOrder           = \(ws:_:t:_) -> [ws,t]
  , ppOutput          = sequence_ . mapM hPutStrLn hs
  , ppSort            = getSortByIndex
  , ppExtras          = []
  }

clickWS f ws = if null ws
  then s
  else buildCmd s
  where
  buildCmd s = "^ca(1," ++ cmd ++ ")" ++ s ++ "^ca()"
  cmd = if isDigit c
    then "xdotool key 'super+" ++ ws ++ "'"
    else "xdotool key 'super+shift+" ++ [toLower c] ++ "'"
  c = head ws 
  s = f ws

myBarConfig = myDzenConfig
  { dzenBg = bg normal
  , dzenFg = fg normal
  }

myXPConfig = defaultXPConfig
  { font = myFont 8
  , bgColor = bg normal
  , fgColor = fg normal
  , fgHLight = fg focused
  , bgHLight = bg focused
  , borderColor = fg borders
  , promptBorderWidth = 1
  , historySize = 100
  }

myDzenConfig :: DzenConfig
myDzenConfig = defaultDzenConfig
  { gap = 50
  , dzenFont  = Just $ myFont 8
  , fontWidth = 13
  , dzenFg = fg cs
  , dzenBg = bg cs
  }
  where
  cs = light

data ColorScheme = CS
  { fg :: String
  , bg :: String
  }

