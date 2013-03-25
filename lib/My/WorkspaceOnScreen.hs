
module My.WorkspaceOnScreen
  ( onScreen
  , workspaceOnScreen
  , WSMap
  ) where

import XMonad
  ( windows
  , screenWorkspace
  , whenJust
  , WorkspaceId
  , ScreenId
  , WindowSet
  , X (..)
  )

import qualified Data.Map as M

type WSMap = M.Map WorkspaceId ScreenId 

onScreen :: ScreenId -> [WorkspaceId] -> WSMap
onScreen i ws = M.fromList [ (w,i) | w <- ws ]

workspaceOnScreen ::
  WSMap -> (WorkspaceId -> WindowSet -> WindowSet) -> WorkspaceId -> X ()
workspaceOnScreen m f w =
  case M.lookup w m of
    Nothing -> return ()
    Just s  ->
      do mws <- screenWorkspace s
         whenJust mws (windows . f)
         windows $ f w

