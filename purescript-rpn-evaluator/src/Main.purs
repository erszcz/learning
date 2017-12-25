module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Partial.Unsafe (unsafePartial)

data Symbol = Add
            | Sub
            | Mul
            | Div
            | Num Int

instance showSymbol :: Show Symbol where
  show Add = "Add"
  show Sub = "Sub"
  show Mul = "Mul"
  show Div = "Div"
  show (Num n) = show n

token :: Partial => String -> Maybe Symbol
token "+" = Just Add
token "-" = Just Sub
token "*" = Just Mul
token "/" = Just Div
token num = map Num (fromString num)

example1 :: String
example1 = "2 3 +"

example2 :: String
example2 = "2 3 + 3 *"

{--parse :: String -> Maybe [Symbol]--}
parse expr = map (unsafePartial token) (split (Pattern " ") expr)

{--main :: forall e. Eff (console :: CONSOLE | e) Unit--}
main = do
  logShow $ unsafePartial $ parse "3 2 +"
