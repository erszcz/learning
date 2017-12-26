module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Int (fromString)
import Data.List (List(..), (:), fromFoldable)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)

data Symbol = Op Operator
            | Num Int

instance showSymbol :: Show Symbol where
  show (Op op) = show op
  show (Num n) = show n

data Operator = Add
              | Sub
              | Mul
              | Div

instance showOperator :: Show Operator where
  show Add = "Add"
  show Sub = "Sub"
  show Mul = "Mul"
  show Div = "Div"

symbol :: String -> Maybe Symbol
symbol "+" = Just $ Op Add
symbol "-" = Just $ Op Sub
symbol "*" = Just $ Op Mul
symbol "/" = Just $ Op Div
symbol num = map Num (fromString num)

example1 :: String
example1 = "2 3 +"

example2 :: String
example2 = "2 3 + 3 *"

example3 :: String
example3 = "2 3 + 3 * 5 /"

example4 :: String
example4 = "5 4 -"

parse :: String -> Maybe (Array Symbol)
parse expr = traverse symbol $ split (Pattern " ") expr

eval :: List Symbol -> Maybe Int
eval expr = go expr Nil
  where go Nil (Cons v Nil) = Just v
        go (Op op : expr') (y : x : stack) = go expr' $ (evalOp op x y) : stack
        go (Num n : expr') stack = go expr' (n : stack)
        go _ _ = Nothing
        evalOp op = case op of
          Add -> (+)
          Sub -> (-)
          Mul -> (*)
          Div -> (/)

parseEval :: String -> Maybe Int
parseEval expr = (parse expr) >>= (eval <<< fromFoldable)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow $ parseEval example4
