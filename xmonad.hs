
-- Imports {{{

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Hooks.ManageHelpers (doCenterFloat,doFullFloat)
import XMonad.Hooks.ManageDocks (avoidStruts,manageDocks)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP)
import XMonad.Hooks.UrgencyHook (withUrgencyHookC,urgencyConfig,UrgencyConfig(..),SuppressWhen(..),dzenUrgencyHook,NoUrgencyHook(..))
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)

import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.IM (withIM, Property(..))
import XMonad.Layout.PerWorkspace (onWorkspace)

import XMonad.Prompt.Shell (split,shellPrompt)
import XMonad.Prompt.Workspace (workspacePrompt)
import XMonad.Prompt.XMonad (xmonadPrompt)

import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.Run (spawnPipe,runProcessWithInput,runInTerm)
import XMonad.Util.Paste (pasteChar)

import XMonad.Actions.SpawnOn (spawnHere)
import XMonad.Actions.Submap (submap)
import XMonad.Actions.FindEmptyWorkspace (viewEmptyWorkspace,tagToEmptyWorkspace)
import XMonad.Actions.WindowGo (runOrRaise,raiseMaybe)
import XMonad.Actions.Warp (warpToWindow)
import XMonad.Actions.CycleWS (toggleWS')

import Control.Applicative ((<$>))
import Control.Monad (void)
import Data.List (isPrefixOf,isInfixOf,intercalate)
import Data.Char (toLower,isDigit)
import Data.Maybe (fromJust)
import System.Exit (exitWith,ExitCode(..))
import Text.ParserCombinators.ReadP (readP_to_S)
import qualified Data.Map as M

import My.Bars
import My.Decor
import My.WorkspaceOnScreen
import My.NamedSubmap
import My.CycleScreens
import My.QueryHelpers
import My.Utils

-- }}}

-- Applications {{{

myExtMon     = "DP2"
myTerminal   = "urxvt"
myBrowser    = "google-chrome"
irssi        = runInTerm "-t irssi" "irssi"
thunderbird  = spawn "thunderbird"
nmApplet     = spawn "nm-applet"

-- }}}

-- Workspaces {{{

wsMap = M.union
  (onScreen 1 ["1","2","3","4","5"])
  (onScreen 0 ["6","7","8","9","M","I"])

myWorkspaces = M.keys wsMap

-- }}}

-- Layouts {{{

myLayout     = avoidStruts $ smartBorders $ 
  --onWorkspace "I" im $
  tiled ||| Mirror tiled ||| Full
  where
  tiled = Tall nmaster delta ratio
  nmaster = 1
  delta = 3 / 100
  ratio = 1 / 2
  im = withIM 0.2 (Role "buddy_list") Full

-- }}}

-- Keys {{{

myModKey     = mod4Mask

allWorkspaces conf p = filter p $ XMonad.workspaces conf
allNumberWorkspaces conf = allWorkspaces conf (\w -> (length w == 1 && isDigit (head w)))
allNamedWorkspaces conf = allWorkspaces conf (\w -> not (length w == 1 && isDigit (head w)))

myKeys conf = mkKeymap conf $
  ---- show keys ----
  ( "M-S-k"          , sm "Keys" topLevelKeys) :
  ---- actual bindings ----
  reduceKeys topLevelKeys ++
  [ ("M-" ++ m ++ w, f w)
  | (f,m) <- [(myView, ""), (myShift, "S-")]
  , w     <- allNumberWorkspaces conf 
  ]
  where
  topLevelKeys =
    ---- nav ----
    [ ( "M-S-m"         , "View Mail"               , workspaceOnScreen wsMap W.view "M" )
    , ( "M-S-i"         , "View IM"                 , workspaceOnScreen wsMap W.view "I" )
    , ( "M-<Tab>"       , "Cycle Windows"           , windows W.focusDown )
    , ( "M-S-<Tab>"     , "Cycle Screens"           , cycleScreensWith myView )
    , ( "M-j"           , "Focus Down"              , windows W.focusUp )
    , ( "M-k"           , "Focus Up"                , windows W.focusDown )
    ---- session ----
    , ( "M-q"           , "Restart XMonad"          , spawn "xmonad --recompile && xmonad --restart" )
    , ( "M-C-q"         , "Logout"                  , io $ exitWith ExitSuccess )
    , ( "M-C-<F4>"      , "Shut Down"               , spawn "poweroff" )
    ---- utils ----
    , ( "M-S-l"         , "Lock"                    , spawn "slock" )
    , ( "<Print>"       , "Screenshot"              , spawn "scrot" )
    , ( volUpKey        , "Vol Up"                  , volUp 5 )
    , ( volDownKey      , "Vol Down"                , volDown 5 )
    ---- submaps ----
    , ( "M-x"           , "Applications Keys"       , sm   "Applications" applicationMap )
    , ( "M-p"           , "Prompts Keys"            , sm   "Prompts"      promptMap )
    , ( "M-w"           , "Windows Mode"            , mode "Windows"      windowMap )
    , ( "M-a"           , "Layouts Mode"            , mode "Layouts"      layoutMap )
    , ( "M-S-w"         , "Workspace Mode"          , mode "Workspaces"   workspaceMap )
    , ( "M-v"           , "Volume Mode"             , mode "Volume"       volumeMap )
    , ( "C-d"           , "Confirm Exit"            , smUrgent "Confirm Exit" exitMap )
    ]
  exitMap =
    [ ( "C-d"           , "Yes, really exit."       , pasteChar controlMask 'd' )
    ]
  volumeMap =
    [ ( plus_key        , "Up"                      , volUp 5 )
    , ( "-"             , "Down"                    , volDown 5)
    ]
  workspaceMap =
    [ ( w               , "View " ++ w              , myView w ) | w <- allNumberWorkspaces conf ] ++
    [ ( "m"             , "View M"                  , myView "M" )
    , ( "i"             , "View I"                  , myView "I" )
    , ( "<Tab>"         , "Cycle Windows"           , windows W.focusDown )
    , ( "S-<Tab>"       , "Cycle Screens"           , cycleScreensWith myView )
    ]
  applicationMap =
    [ ( "<Return>"      , "Terminal"                , spawn myTerminal )
    , ( "b"             , "Browser"                 , spawn myBrowser )
    , ( "p"             , "DMenu"                   , spawn "dmenu_run -p '>>>'" )
    , ( "c"             , "Qalculate"               , spawn "qalculate-gtk" )
    , ( "l"             , "GColor"                  , spawn "gcolor2" )
    , ( "v"             , "Evince"                  , spawn "evince" )
    , ( "o"             , "Xprop"                   , spawn "xprop > /home/kcarter/.xprop" )
    , ( "x"             , "Kill Window"             , kill )
    , ( "M-x"           , "Kill Window"             , kill )
    ]
  windowMap =
    [ ( "<Return>"      , "Swap Master"             , windows W.swapMaster )
    , ( "S-j"           , "Swap Down"               , windows W.swapDown )
    , ( "S-k"           , "Swap Up"                 , windows W.swapUp )
    , ( "h"             , "Shrink"                  , sendMessage Shrink )
    , ( "l"             , "Expand"                  , sendMessage Expand )
    , ( plus_key        , "Inc # Master"            , sendMessage (IncMasterN 1) )
    , ( "-"             , "Dec # Master"            , sendMessage (IncMasterN (-1)) )
    ]
  promptMap =
    [ ( "p"             , "WS Prompt (view)"        , workspacePrompt myXPConfig (windows . W.view) )
    , ( "S-p"           , "WS Prompt (shift)"       , workspacePrompt myXPConfig (windows . W.shift) )
    , ( "s"             , "Shell Prompt"            , shellPrompt myXPConfig )
    , ( "x"             , "XMonad Prompt"           , xmonadPrompt myXPConfig )
    , ( "e"             , "View Empty WS"           , viewEmptyWorkspace )
    , ( "S-e"           , "Shift to Empty WS"       , tagToEmptyWorkspace )
    , ( "S-m"           , "Shift to Mail"           , windows $ W.shift "M" )
    , ( "S-i"           , "Shift to IM"             , windows $ W.shift "I" )
    ]
  layoutMap =
    [ ( "<Return>"      , "Sink Window"             , withFocused $ windows . W.sink )
    , ( "a"             , "Next Layout"             , sendMessage NextLayout )
    , ( "M-a"           , "Next Layout"             , sendMessage NextLayout )
    , ( "r"             , "First Layout"            , sendMessage FirstLayout )
    ]
  mode = modeSM mySMConfig conf Nothing
  sm = namedSM mySMConfig conf
  myView w = workspaceOnScreen wsMap W.view w >> warpToWindow 0.5 0.5
  myShift w = (windows $ W.shift w) >> warpToWindow 0.5 0.5
  volUpKey = "<XF86AudioRaiseVolume>"
  volDownKey = "<XF86AudioLowerVolume>"
  plus_key = "S-="
  smUrgent = namedSM (mySMConfig { bgDzen = bg urgent }) conf

-- }}}

-- StartupHook {{{

myStartupHook = do
  runMaybe irssi       (name      =~? "irssi")
  runMaybe thunderbird (className =~? "thunderbird")
  runMaybe nmApplet    (name      =~? "nm")

-- }}}

-- ManageHook {{{

myManageHook = (composeAll . concat $
  [ [ resource   =? r    --> doIgnore          | r <- ignores ]
  , [ role      =~? r    --> doCenterFloat     | r <- matchFloats ]
  , [ appName   =~? n    --> doCenterFloat     | n <- matchFloats ]
  , [ name      =~? n    --> doCenterFloat     | n <- matchFloats ]
  , [ className  =? c    --> doCenterFloat     | c <- classFloats ]
  , [ name      =~? n    --> doFullFloat       | n <- fullFloats ]
  , [ name      =~? n    --> doCenterFloat     | n <- notifications ]
  , [ className  =? c    --> doShift "M"       | c <- mail ]
  , [ name      =~? n    --> doShift "I"       | n <- im ]
  ]) <+> manageDocks
  where
    ignores       = ["desktop_window","stalonetray"]
    classFloats   = ["MPlayer","Xmessage", "Gcolor2"]
    fullFloats    = ["exe"]
    matchFloats   = ["dialog","preferences","settings","wicd","options","contact","nm","qalc","pop-up","task","setup","msg"]
    notifications = ["notify-dzen"]
    mail          = ["Thunderbird"]
    im            = ["irssi","pidgin"]

-- }}}

-- Urgency Config {{{

myUrgencyConfig = urgencyConfig { suppressWhen = Focused }

-- }}}

-- Main {{{

main = do
  cleanUp
  dzenMain <- statusBarMain True
  ext      <- connectedToExt myExtMon
  dzenExt  <- statusBarExternal ext
  xmonad $ withUrgencyHookC NoUrgencyHook myUrgencyConfig $
    defaultConfig
      { modMask            = myModKey
      , terminal           = myTerminal
      , borderWidth        = myBorderWidth
      , normalBorderColor  = bg borders
      , focusedBorderColor = fg borders
      , keys               = myKeys
      , workspaces         = myWorkspaces
      , layoutHook         = myLayout
      , manageHook         = myManageHook <+> manageDocks
      , logHook            = dynamicLogWithPP $ myDzenPP (dzenMain++dzenExt)
      , startupHook        = myStartupHook
      , handleEventHook    = fullscreenEventHook
      }

-- }}}

-- IO {{{

cleanUp :: IO ()
cleanUp = void $ runProcessWithInput "killall" ["stalonetray","dzen2","conky"] ""

-- }}}

-- Helpers {{{

volUp = vol True
volDown = vol False

vol :: Bool -> Int -> X ()
vol isUp delta = spawn ("amixer set Master " ++ show delta ++ "%" ++ dir isUp)
  where
  dir = branch "+" "-"

-- }}}

