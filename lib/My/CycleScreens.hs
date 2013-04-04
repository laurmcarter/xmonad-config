
module My.CycleScreens
  ( cycleScreens
  , cycleScreensWith
  , reportCycleScreens
  , reportCycleScreensWith
  ) where

import XMonad
import XMonad.StackSet

import Control.Monad (void)

-- | Cycle through Xinerama screens using a provided viewing function.
cycleScreensWith :: (WorkspaceId -> X ()) -> X ()
cycleScreensWith f = void $ reportCycleScreensWith f

-- | Cycle through Xinerama screens with standard view function.
cycleScreens :: X ()
cycleScreens = void reportCycleScreens

-- | Cycle through Xinerama screens with standard view function, returning
--   True when cycling back to screen 0.
reportCycleScreens :: X Bool
reportCycleScreens = reportCycleScreensWith viewWS

-- | Cycle through Xinerama screens with provided view function, returning
--   True when cycling back to screen 0.
reportCycleScreensWith :: (WorkspaceId -> X ()) -> X Bool
reportCycleScreensWith f = do
  sid <- gets (screen . current . windowset)
  ws <- screenWorkspace $ sid + 1
  maybe (screenWorkspace 0 >>= flip whenJust f) f ws
  case ws of
    Just _  -> return True
    Nothing -> return False

viewWS :: WorkspaceId -> X ()
viewWS = windows . view

