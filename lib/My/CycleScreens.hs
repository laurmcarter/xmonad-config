
module My.CycleScreens
  ( cycleScreens
  , cycleScreensWith
  ) where

import XMonad
import XMonad.StackSet

cycleScreensWith f = do
  sid <- gets (screen . current . windowset)
  ws <- screenWorkspace $ sid + 1
  maybe (screenWorkspace 0 >>= flip whenJust f) f ws

cycleScreens = cycleScreensWith viewWS

viewWS :: WorkspaceId -> X ()
viewWS = windows . view

