
module My.Decor
  ( normal
  , urgent
  , focused
  , borders
  , myXPConfig
  , myDzenPP
  , mySMConfig
  , myFont
  , myBorderWidth
  , dzenColor
  , ColorScheme(..)
  ) where

import XMonad.Prompt
import XMonad.Hooks.DynamicLog
import XMonad.Util.WorkspaceCompare

import My.NamedSubmap

import Data.Char
import Data.Word (Word32)
import System.IO (hPutStrLn)

myBorderWidth :: Word32
myBorderWidth = 1
myFont :: Int -> String
myFont s       = "-*-terminus-medium-r-normal-*-" ++ show s ++ "-*-*-*-*-*-*-*"
--myFont        = "Droid Sans Mono:size=8"

normal  = CS { fg = "#ffffff" , bg = "#000000" }
urgent  = CS { fg = "#ffffff" , bg = "#ff0000" }
focused = CS { fg = "#f0f0f0" , bg = "#333333" }
borders = CS { fg = "#433ffe" , bg = "#010062" }

myDzenPP hs = PP 
  { ppCurrent         = clickWS (dzenColor "lightgreen" "" . wrap "[" "]")
  , ppVisible         = clickWS (wrap "<" ">")
  , ppHidden          = clickWS (dzenColor "grey" "" . pad)
  , ppHiddenNoWindows = const ""
  , ppUrgent          = clickWS (dzenColor (fg urgent) (bg urgent) . pad)
  , ppSep             = " : "
  , ppWsSep           = " "
  , ppTitle           = shorten 80
  , ppLayout          = id
  , ppOrder           = id
  , ppOutput          = sequence_ . mapM hPutStrLn hs
  , ppSort            = getSortByIndex
  , ppExtras          = []
  }

clickWS f ws = if (length ws == 0)
  then s
  else buildCmd s
  where
  buildCmd s = "^ca(1," ++ cmd ++ ")" ++ s ++ "^ca()"
  cmd = if (isDigit c)
    then "xdotool key 'super+" ++ ws ++ "'"
    else "xdotool key 'super+shift+" ++ [toLower c] ++ "'"
  c = head ws 
  s = f ws


myXPConfig = defaultXPConfig
  { font = myFont 12
  , bgColor = bg normal
  , fgColor = fg normal
  , fgHLight = fg focused
  , bgHLight = bg focused
  , borderColor = fg borders
  , promptBorderWidth = 1
  , historySize = 100
  }

mySMConfig :: SMConfig
mySMConfig = defaultSMConfig
  { gap = 50
  , dzenFont  = Just $ myFont 14
  , fontWidth = 13
  , fgDzen = fg cs
  , bgDzen = bg cs
  }
  where
  cs = focused

data ColorScheme = CS
  { fg :: String
  , bg :: String
  }

