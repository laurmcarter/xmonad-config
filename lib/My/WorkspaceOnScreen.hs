
module My.WorkspaceOnScreen
  ( onScreen
  , workspaceOnScreen
  ) where

import XMonad (windows,screenWorkspace,whenJust)

onScreen i ws = [ (w,i) | w <- ws ]

workspaceOnScreen m f w =
  case lookup w m of
    Nothing -> return ()
    Just s  ->
      do mws <- screenWorkspace s
         whenJust mws (windows . f)
         windows $ f w

