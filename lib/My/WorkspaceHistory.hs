{-# LANGUAGE DeriveDataTypeable #-}

module My.WorkspaceHistory
  ( back
  , backSave
  , forward
  , forwardSave
  , view
  , displayStack
  ) where

import XMonad
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as W

import Control.Applicative
import Control.Monad.State
import Data.Maybe
import Data.Monoid
import Data.Data
import qualified Data.Map as M

-- Types {{{

data WSHistory = WSH
  { wsHistory :: [WorkspaceId]
  , wsFuture  :: [WorkspaceId]
  } deriving (Eq,Show,Typeable,Data)

type Getter = WSHistory -> [WorkspaceId]
type Setter = [WorkspaceId] -> WSHistory -> WSHistory
type PeekOrPop = X (Maybe WorkspaceId)

setHistory :: Setter
setHistory h (WSH _ f) = WSH h f

setFuture :: Setter
setFuture f (WSH h _) = WSH h f

instance ExtensionClass WSHistory where
  initialValue = WSH [] []

-- }}}

-- Stack Ops {{{

push :: Getter -> Setter -> WorkspaceId -> X ()
push g s w = do
  wsh <- XS.get
  let ws  = g wsh
      ws' = if length ws >= 500 then take 99 ws else ws
  XS.put $ s (w:ws') wsh

pushHist :: WorkspaceId -> X ()
pushHist = push wsHistory setHistory

pushFut :: WorkspaceId -> X ()
pushFut = push wsFuture setFuture

pop :: Getter -> Setter -> X (Maybe WorkspaceId)
pop g s = do
  wsh <- XS.get
  case g wsh of
    []   -> return Nothing
    w:ws -> do
      XS.put $ s ws wsh
      return $ Just w

popHist :: X (Maybe WorkspaceId)
popHist = pop wsHistory setHistory

popFut :: X (Maybe WorkspaceId)
popFut = pop wsFuture setFuture

peek :: Getter -> X (Maybe WorkspaceId)
peek f = listToMaybe . f <$> XS.get

peekHist :: X (Maybe WorkspaceId)
peekHist = peek wsHistory

peekFut :: X (Maybe WorkspaceId)
peekFut = peek wsFuture

clear :: Setter -> X ()
clear f = XS.put . f [] =<< XS.get

clearHist :: X ()
clearHist = clear setHistory

clearFut :: X ()
clearFut = clear setFuture

-- }}}

displayStack :: X ()
displayStack = do
  s@(WSH _ _) <- XS.get
  spawn ("echo \"Workspace History: " ++ show s ++ "\" | xmessage -file -")

move :: Bool -> Bool -> (WorkspaceId -> X ()) -> X ()
move forward shouldPop viewF = do
  mw <- popOrPeek
  whenJust mw $ \w -> do
    cur <- W.currentTag . windowset <$> get
    when shouldPop $ pushF cur
    s@(WSH _ _) <- XS.get
    viewF w
  where
  popOrPeek :: X (Maybe WorkspaceId)
  popOrPeek = if shouldPop then popF else peekF
  popF = if forward then popFut else popHist
  peekF = if forward then peekFut else peekHist
  pushF = if forward then pushHist else pushFut

back :: (WorkspaceId -> X ()) -> X ()
back = move False True

backSave :: (WorkspaceId -> X ()) -> X ()
backSave = move False False

forward :: (WorkspaceId -> X ()) -> X ()
forward = move True True

forwardSave :: (WorkspaceId -> X ()) -> X ()
forwardSave = move True False

view :: (WorkspaceId -> X ()) -> WorkspaceId -> X ()
view viewF w = do
  clearFut
  cur <- W.currentTag . windowset <$> get
  unless (cur == w) $ do
    s@(WSH _ _) <- XS.get
    pushHist cur
    viewF w

