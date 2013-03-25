{-# LANGUAGE DeriveDataTypeable #-}

module My.WorkspaceHistory
  ( back
  , forward
  , view
  ) where

import XMonad
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as W

import Control.Applicative
import Control.Monad.State
import Data.Monoid
import Data.Data
import qualified Data.Map as M

data WSHistory = WSH
  { wsHistory :: [WorkspaceId]
  , wsFuture  :: [WorkspaceId]
  } deriving (Eq,Show,Typeable,Data)

instance ExtensionClass WSHistory where
  initialValue = WSH [] []

pushWSHist :: WorkspaceId -> X ()
pushWSHist w = do
  (WSH h f) <- XS.get
  let h' = if length h >= 500 then take 99 h else h
  XS.put $ WSH (w:h') f

pushWSFut :: WorkspaceId -> X ()
pushWSFut w = do
  (WSH h f) <- XS.get
  let f' = if length f >= 500 then take 99 f else f
  XS.put $ WSH h (w:f')

popWSHist :: X (Maybe WorkspaceId)
popWSHist = do
  (WSH h f) <- XS.get
  case h of
    []   -> return Nothing
    w:h' -> do
      XS.put $ WSH h' f
      return (Just w)

popWSFut :: X (Maybe WorkspaceId)
popWSFut = do
  (WSH h f) <- XS.get
  case f of
    []   -> return Nothing
    w:f' -> do
      XS.put $ WSH h f'
      return (Just w)

clearWSFut :: X ()
clearWSFut = do
  (WSH h _) <- XS.get
  XS.put $ WSH h []

back :: (WorkspaceId -> X ()) -> X ()
back f = do
  mw <- popWSHist
  case mw of
    Nothing -> do
      -- spawn ("echo \"tried back, but history was empty\" | xmessage -file -")
      return ()
    Just w  -> do
      cur <- W.currentTag . windowset <$> get
      pushWSFut cur
      s@(WSH _ _) <- XS.get
      -- spawn ("echo \"back " ++ show w ++ ", " ++ show s ++ "\" | xmessage -file -")
      f w

forward :: (WorkspaceId -> X ()) -> X ()
forward f = do
  mw <- popWSFut
  case mw of
    Nothing -> do
      -- spawn ("echo \"tried forward, but future was empty\" | xmessage -file -")
      return ()
    Just w  -> do
      cur <- W.currentTag . windowset <$> get
      pushWSHist cur
      s@(WSH _ _) <- XS.get
      -- spawn ("echo \"forward " ++ show w ++ ", " ++ show s ++ "\" | xmessage -file -")
      f w

view :: (WorkspaceId -> X ()) -> WorkspaceId -> X ()
view f w = do
  clearWSFut
  cur <- W.currentTag . windowset <$> get
  if cur == w
  then return ()
  else do
    s@(WSH _ _) <- XS.get
    pushWSHist cur
    -- spawn ("echo \"view " ++ show cur ++ ", " ++ show s ++ "\" | xmessage -file -")
    f w

