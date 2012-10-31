
module My.CycleScreens
  ( cycleScreens
  ) where

import XMonad
import XMonad.StackSet

cycleScreens = do
  sid <- gets (screen . current . windowset)
  ws <- screenWorkspace $ sid + 1
  maybe (screenWorkspace 0 >>= viewIfExists) viewWS ws

viewWS :: WorkspaceId -> X ()
viewWS = windows . view

viewIfExists :: Maybe WorkspaceId -> X ()
viewIfExists = flip whenJust viewWS

