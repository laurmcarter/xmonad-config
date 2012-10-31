
import XMonad
import qualified XMonad.StackSet as W

import XMonad.Hooks.ManageHelpers (doCenterFloat,doFullFloat)
import XMonad.Hooks.ManageDocks (avoidStruts,manageDocks)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP)

import XMonad.Layout.NoBorders (smartBorders)

import XMonad.Prompt.Shell (split,shellPrompt)
import XMonad.Prompt.Workspace (workspacePrompt)
import XMonad.Prompt.XMonad (xmonadPrompt)

import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.Run (spawnPipe,runProcessWithInput)

import XMonad.Actions.SpawnOn (spawnHere)
import XMonad.Actions.Submap (submap)
import XMonad.Actions.FindEmptyWorkspace (viewEmptyWorkspace,tagToEmptyWorkspace)
import XMonad.Actions.CycleWS (toggleWS)

import Data.List (isPrefixOf,isInfixOf,intercalate)
import Data.Char (toLower)
import Data.Maybe (fromJust)
import System.Exit (exitWith,ExitCode(..))

import My.Bars
import My.Decor
import My.WorkspaceOnScreen
import My.NamedSubmap

myModKey     = mod4Mask
myTerminal   = "gnome-terminal"
myBrowser    = "google-chrome"

wsMap = (onScreen 0 ["1","2","3","4","M","I"]) ++
        (onScreen 1 ["5","6","7","8","9"])

myWorkspaces = (map show [1..9]) ++ ["M","I"]

myLayout     = tiled ||| Mirror tiled ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    delta = 3 / 100
    ratio = 1 / 2

myKeys conf = mkKeymap conf $
    ---- app keys ----
    [ ( "M-x"          , sm "Applications" $
      [ ( "<Return>"   , "Terminal"    , spawn myTerminal )
      , ( "b"          , "Browser"     , spawn myBrowser )
      , ( "p"          , "DMenu"       , spawn "dmenu_run -p '>>>'" )
      , ( "c"          , "Qalculate"   , spawn "qalculate-gtk" )
      , ( "l"          , "GColor"      , spawn "gcolor2" )
      , ( "v"          , "Evince"      , spawn "evince" )
      , ( "o"          , "Xprop"       , spawn "xprop > /home/kcarter/.xprop" )
      , ( "x"          , "Kill Window" , kill )
      , ( "M-x"        , "Kill Window" , kill )
      ] )
    ---- window keys ----
    , ( "M-w"          , sm "Windows" $
      [ ( "<Return>"   , "Swap Master"  , windows W.swapMaster )
      , ( "S-j"        , "Swap Down"    , windows W.swapDown )
      , ( "S-k"        , "Swap Up"      , windows W.swapUp )
      , ( "j"          , "Focus Up"     , windows W.focusUp )
      , ( "k"          , "Focus Down"   , windows W.focusDown )
      , ( "h"          , "Shrink"       , sendMessage Shrink )
      , ( "l"          , "Expand"       , sendMessage Expand )
      , ( "+"          , "Inc # Master" , sendMessage (IncMasterN 1) )
      , ( "-"          , "Dec # Master" , sendMessage (IncMasterN (-1)) )
      ] )
    ---- workspace keys ----
    , ( "M-s"          , sm "Prompts" $
      [ ( "p"          , "WS Prompt (view)"  , workspacePrompt myXPConfig (windows . W.view) )
      , ( "S-p"        , "WS Prompt (shift)" , workspacePrompt myXPConfig (windows . W.shift) )
      , ( "s"          , "Shell Prompt"      , shellPrompt myXPConfig )
      , ( "x"          , "XMonad Prompt"     , xmonadPrompt myXPConfig )
      , ( "e"          , "View Empty WS"     , viewEmptyWorkspace )
      , ( "S-e"        , "Shift to Empty WS" , tagToEmptyWorkspace )
      , ( "S-m"        , "Shift to Mail"     , windows $ W.shift "M" )
      , ( "S-i"        , "Shift to IM"       , windows $ W.shift "I" )
      ] )
    ---- layout keys ----
    , ( "M-a"          , sm "Layouts" $
      [ ( "<Return>"   , "Sink Window"  , withFocused $ windows . W.sink )
      , ( "a"          , "Next Layout"  , sendMessage NextLayout )
      , ( "M-a"        , "Next Layout"  , sendMessage NextLayout )
      , ( "r"          , "First Layout" , sendMessage FirstLayout )
      ] )
    , ( "M-f"          , sm "Finch" $
      [ ( "<Tab>"      , "Next"    , withFocused $ xDoTool "alt+n" )
      , ( "S-<Tab>"    , "Prev"    , withFocused $ xDoTool "alt+p" )
      , ( "m"          , "Move"    , withFocused $ xDoTool "alt+m" )
      , ( "r"          , "Resize"  , withFocused $ xDoTool "alt+r" )
      , ( "c"          , "Close"   , withFocused $ xDoTool "alt+c" )
      , ( "q"          , "Quit"    , withFocused $ xDoTool "alt+q" )
      , ( "a"          , "Options" , withFocused $ xDoTool "alt+a" )
      ] )
    ---- top level keys ----
    , ( "M-k"          , sm "Keys" topLevelKeys)
    ] ++
    reduceKeys topLevelKeys ++
    [ ("M-" ++ m ++ w, f w)
      | (f,m) <- [(myView, ""), (myShift, "S-")]
      , w     <- take 9 $ XMonad.workspaces conf
    ]
    where
      topLevelKeys =
        [ ( "M-S-m"        , "View Mail"          , workspaceOnScreen wsMap W.view "M" )
        , ( "M-S-i"        , "View IM"            , workspaceOnScreen wsMap W.view "I" )
        , ( "<Print>"      , "Screenshot"         , spawn "scrot" )
        , ( "M-S-l"        , "Lock"               , spawn "slock" )
        , ( "M-<Tab>"      , "Cycle Windows"      , windows W.focusDown )
        , ( "M-S-<Tab>"    , "Last WS"            , toggleWS )
        , ( "M-q"          , "Restart XMonad"     , spawn "xmonad --restart" )
        , ( "M-S-q"        , "Logout"             , io (exitWith ExitSuccess))
        , ( "M-S-<F4>"     , "Shut Down"          , spawn "poweroff" )
        , ( "<XF86AudioRaiseVolume>" , "Vol Up"   , spawn $ vol 5 True )
        , ( "<XF86AudioLowerVolume>" , "Vol Down" , spawn $ vol 5 False )
        ]
      sm = namedSM mySMConfig conf

mySMConfig = defaultSMConfig
  { gap = 50
  , fontWidth = 6
  , font = Just myFont
  }

myView = workspaceOnScreen wsMap W.view
myShift = windows . W.shift

myManageHook = (composeAll . concat $
  [ [ resource  =? r    --> doIgnore          | r <- ignores ]
  , [ role     =~? r    --> doCenterFloat     | r <- matchFloats ]
  , [ appName  =~? n    --> doCenterFloat     | n <- matchFloats ]
  , [ className =? c    --> doCenterFloat     | c <- classFloats ]
  , [ name     =~? n    --> doFullFloat       | n <- fullFloats ]
  , [ name     =~? n    --> doCenterFloat     | n <- notifications ]
  , [ className =? c    --> doShift "M"       | c <- mail ]
  , [ name     =~? n    --> doShift "I"       | n <- im ]
  ]) <+> manageDocks
  where
    ignores       = ["desktop_window","stalonetray"]
    classFloats   = ["MPlayer","Xmessage", "Gcolor2"]
    fullFloats    = ["exe"]
    matchFloats   = ["dialog","preferences","settings","wicd","options","contact","nm","qalc","pop-up"]
    notifications = ["notify-dzen"]
    mail          = ["Thunderbird"]
    im            = ["finch"]
    ----
    role = stringProperty "WM_WINDOW_ROLE"
    name = stringProperty "WM_NAME"
    ----
    (=/?)   :: Query String -> String -> Query Bool
    x =/? q  = fmap not $ x =? q
    (=~?)   :: Query String -> String -> Query Bool
    q =~? x  = fmap (isInfixOf $ decap x) $ fmap decap q
    decap   :: String -> String
    decap    = map toLower

main = do
  cleanUp
  dzenMain <- statusBarMain True
  ext      <- connectedToExt
  dzenExt  <- statusBarExternal ext
  mapM_ spawn myStartApps
  xmonad $ defaultConfig
    { modMask            = myModKey
    , terminal           = myTerminal

    , borderWidth        = myBorderWidth
    , normalBorderColor  = bg borders
    , focusedBorderColor = fg borders

    , keys               = myKeys
    , workspaces         = myWorkspaces
    , layoutHook         = avoidStruts $ smartBorders $ myLayout
    , manageHook         = myManageHook <+> manageDocks
    , logHook            = dynamicLogWithPP $ myDzenPP (dzenMain++dzenExt)
    }

-- IO

connectedToExt :: IO Bool
connectedToExt = do
  o <- pipe [ ("xrandr",[]) ] ""
  let ls = between "VGA1" "HDMI1" $ split '\n' o
  return $ any (elem '*') ls
  where
    between b a =
      reverse . dropWhile (not . isPrefixOf a) .
        reverse . dropWhile (not . isPrefixOf b)

vol delta up = "amixer set Master " ++ show delta ++ "%" ++ if up then "+" else "-"

--xDoTool k = spawn ("xdotool key " ++ k)
xDoTool k w = io $ do
  runProcessWithInput "xdotool" ["text","--window",show w,k] ""
  trace ("XMONAD: " ++ show w)

cleanUp :: IO ()
cleanUp = runProcessWithInput "killall" ["stalonetray","dzen2","conky"] "" >> return ()

-- Helpers

stackToList :: W.Stack a -> [a]
stackToList s = (reverse $ W.up s) ++ [W.focus s] ++ W.down s

actualWS s ss = case filter ((s ==) . W.tag) $ W.workspaces ss of
  []    -> Nothing
  (w:_) -> Just w

getNames ws = mapM (\w->runProcessWithInput "xdotool" ["getwindowname",show w] "") ws

windowsOfWS ws ss = case actualWS ws ss of
  Nothing -> []
  Just w  -> case W.stack w of
    Nothing   -> []
    Just wins -> stackToList wins

pipe :: [(String,[String])] -> String -> IO String
pipe [] res              = return res
pipe ((c,args):rest) res = runProcessWithInput c args res >>= pipe rest

