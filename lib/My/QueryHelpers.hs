
module My.QueryHelpers
  ( role
  , name
  , (=/?)
  , (=~?)
  , runMaybe
  ) where

import XMonad (Query(..),X(..))
import XMonad.ManageHook (stringProperty,(=?),idHook)
import XMonad.Actions.WindowGo (ifWindow)

import Data.Char (toLower)
import Data.List (isInfixOf)

role = stringProperty "WM_WINDOW_ROLE"
name = stringProperty "WM_NAME"

(=/?)   :: Query String -> String -> Query Bool
x =/? q  = fmap not $ x =? q

(=~?)   :: Query String -> String -> Query Bool
q =~? x  = fmap (isInfixOf $ decap x) $ fmap decap q

decap   :: String -> String
decap    = map toLower

runMaybe :: X () -> Query Bool -> X ()
runMaybe f qry = ifWindow qry idHook f
