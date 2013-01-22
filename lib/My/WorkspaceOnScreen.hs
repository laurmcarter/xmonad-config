
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

import qualified Data.Map as Map

type WSMap = Map.Map WorkspaceId ScreenId 

onScreen :: ScreenId -> [WorkspaceId] -> WSMap
onScreen i ws = Map.fromList [ (w,i) | w <- ws ]

workspaceOnScreen ::
  WSMap -> (WorkspaceId -> WindowSet -> WindowSet) -> WorkspaceId -> X ()
workspaceOnScreen m f w =
  case Map.lookup w m of
    Nothing -> return ()
    Just s  ->
      do mws <- screenWorkspace s
         whenJust mws (windows . f)
         windows $ f w

