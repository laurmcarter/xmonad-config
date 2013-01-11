
-- Imports {{{

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Hooks.ManageHelpers (doCenterFloat,doFullFloat)
import XMonad.Hooks.ManageDocks (avoidStruts,manageDocks)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP)
import XMonad.Hooks.UrgencyHook (withUrgencyHook,NoUrgencyHook(..))

import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.IM (withIM, Property(..))
import XMonad.Layout.PerWorkspace (onWorkspace)

import XMonad.Prompt.Shell (split,shellPrompt)
import XMonad.Prompt.Workspace (workspacePrompt)
import XMonad.Prompt.XMonad (xmonadPrompt)

import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.Run (spawnPipe,runProcessWithInput,runInTerm)

import XMonad.Actions.SpawnOn (spawnHere)
import XMonad.Actions.Submap (submap)
import XMonad.Actions.FindEmptyWorkspace (viewEmptyWorkspace,tagToEmptyWorkspace)
import XMonad.Actions.WindowGo (runOrRaise,raiseMaybe)
import XMonad.Actions.Warp (warpToWindow)
import XMonad.Actions.CycleWS (toggleWS')

import Data.List (isPrefixOf,isInfixOf,intercalate)
import Data.Char (toLower)
import Data.Maybe (fromJust)
import System.Exit (exitWith,ExitCode(..))

import My.Bars
import My.Decor
import My.WorkspaceOnScreen
import My.NamedSubmap
import My.CycleScreens
import My.QueryHelpers

-- }}}

-- Applications {{{

myTerminal   = "gnome-terminal"
myBrowser    = "google-chrome"
finch        = runInTerm "--geometry 1278x782 -t finch" "finch"
pidgin       = spawn "pidgin"
thunderbird  = spawn "thunderbird"
nmApplet     = spawn "nm-applet"

-- }}}

-- Workspaces {{{

wsMap = (onScreen 1 ["1","2","3","4","5"]) ++
        (onScreen 0 ["6","7","8","9","M","I"])

myWorkspaces = (map show [1..9]) ++ ["M","I"]

-- }}}

-- Layouts {{{

myLayout     = avoidStruts $ smartBorders $ 
  onWorkspace "I" im $
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

myKeys conf = mkKeymap conf $
    ---- show keys ----
    ( "M-k"          , sm "Keys" topLevelKeys) :
    ---- actual bindings ----
    reduceKeys topLevelKeys ++
    [ ("M-" ++ m ++ w, f w)
      | (f,m) <- [(myView, ""), (myShift, "S-")]
      , w     <- take 9 $ XMonad.workspaces conf
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
        , ( "M-q"           , "Restart XMonad"          , spawn "xmonad --restart" )
        , ( "M-C-q"         , "Logout"                  , io $ exitWith ExitSuccess )
        , ( "M-C-<F4>"      , "Shut Down"               , spawn "poweroff" )
        ---- utils ----
        , ( "M-S-l"         , "Lock"                    , spawn "slock" )
        , ( "<Print>"       , "Screenshot"              , spawn "scrot" )
        , ( "<XF86AudioRaiseVolume>" , "Vol Up"         , spawn $ volUp 5 )
        , ( "<XF86AudioLowerVolume>" , "Vol Down"       , spawn $ volDown 5 )
        ---- submaps ----
        , ( "M-x"          , "Applications Keys"        , sm "Applications" applicationMap )
        , ( "M-w"          , "Windows Keys"             , sm "Windows" windowMap )
        , ( "M-s"          , "Prompts Keys"             , sm "Prompts" workspaceMap )
        , ( "M-a"          , "Layouts Keys"             , sm "Layouts" layoutMap )
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
        [ ( "<Return>"     , "Swap Master"              , windows W.swapMaster )
        , ( "S-j"          , "Swap Down"                , windows W.swapDown )
        , ( "S-k"          , "Swap Up"                  , windows W.swapUp )
        , ( "h"            , "Shrink"                   , sendMessage Shrink )
        , ( "l"            , "Expand"                   , sendMessage Expand )
        , ( "+"            , "Inc # Master"             , sendMessage (IncMasterN 1) )
        , ( "-"            , "Dec # Master"             , sendMessage (IncMasterN (-1)) )
        ]
      workspaceMap =
        [ ( "p"          , "WS Prompt (view)"           , workspacePrompt myXPConfig (windows . W.view) )
        , ( "S-p"        , "WS Prompt (shift)"          , workspacePrompt myXPConfig (windows . W.shift) )
        , ( "s"          , "Shell Prompt"               , shellPrompt myXPConfig )
        , ( "x"          , "XMonad Prompt"              , xmonadPrompt myXPConfig )
        , ( "e"          , "View Empty WS"              , viewEmptyWorkspace )
        , ( "S-e"        , "Shift to Empty WS"          , tagToEmptyWorkspace )
        , ( "S-m"        , "Shift to Mail"              , windows $ W.shift "M" )
        , ( "S-i"        , "Shift to IM"                , windows $ W.shift "I" )
        ]
      layoutMap =
        [ ( "<Return>"   , "Sink Window"                , withFocused $ windows . W.sink )
        , ( "a"          , "Next Layout"                , sendMessage NextLayout )
        , ( "M-a"        , "Next Layout"                , sendMessage NextLayout )
        , ( "r"          , "First Layout"               , sendMessage FirstLayout )
        ]
      sm = namedSM mySMConfig conf
      myView w = workspaceOnScreen wsMap W.view w >> warpToWindow 0.5 0.5
      myShift w = (windows $ W.shift w) >> warpToWindow 0.5 0.5

-- }}}

-- StartupHook {{{

myStartupHook = do
  pidgin
  --runMaybe pidgin      (className =~? "pidgin")
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
  , [ className =~? n    --> doShift "I"       | n <- im ]
  ]) <+> manageDocks
  where
    ignores       = ["desktop_window","stalonetray"]
    classFloats   = ["MPlayer","Xmessage", "Gcolor2"]
    fullFloats    = ["exe"]
    matchFloats   = ["dialog","preferences","settings","wicd","options","contact","nm","qalc","pop-up","task","setup","msg"]
    notifications = ["notify-dzen"]
    mail          = ["Thunderbird"]
    im            = ["finch","pidgin"]

-- }}}

-- Main {{{

main = do
  cleanUp
  dzenMain <- statusBarMain True
  ext      <- connectedToExt
  dzenExt  <- statusBarExternal ext
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
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
    }

-- }}}

-- IO {{{

connectedToExt :: IO Bool
connectedToExt = do
  o <- pipe [ ("xrandr",[]) ] ""
  let ls = between "VGA1" "HDMI1" $ split '\n' o
  return $ any (elem '*') ls
  where
    between b a =
      reverse . dropWhile (not . isPrefixOf a) .
        reverse . dropWhile (not . isPrefixOf b)

cleanUp :: IO ()
cleanUp = runProcessWithInput "killall" ["stalonetray","dzen2","conky"] "" >> return ()

-- }}}

-- Helpers {{{

pipe :: [(String,[String])] -> String -> IO String
pipe [] res              = return res
pipe ((c,args):rest) res = runProcessWithInput c args res >>= pipe rest

volUp d = vol d True
volDown d = vol d False
vol delta up = "amixer set Master " ++ show delta ++ "%" ++ if up then "+" else "-"

-- }}}

