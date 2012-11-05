
module My.CycleScreens
  ( cycleScreens
  , cycleScreensWith
  , reportCycleScreens
  , reportCycleScreensWith
  ) where

import XMonad
import XMonad.StackSet

cycleScreensWith f = reportCycleScreensWith f >> return ()

cycleScreens = reportCycleScreens >> return ()

reportCycleScreens = reportCycleScreensWith viewWS

reportCycleScreensWith f = do
  sid <- gets (screen . current . windowset)
  ws <- screenWorkspace $ sid + 1
  maybe (screenWorkspace 0 >>= flip whenJust f) f ws
  case ws of
    Just _  -> return True
    Nothing -> return False

viewWS :: WorkspaceId -> X ()
viewWS = windows . view

