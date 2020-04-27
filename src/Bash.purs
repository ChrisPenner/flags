module Bash where

import Prelude
import Control.Monad.Writer (Writer, censor, tell)
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll)
import Data.Array (replicate)

type Bash a = Writer String a

caseOption :: String -> Bash Unit -> Bash Unit
caseOption opt inside = do
  line (opt <> ")")
  indented 1 $ do
     inside
  line ";;"
  line ""

case_ :: String -> Bash Unit -> Bash Unit
case_ arg inside = do
  line $ "case " <> arg <> " in"
  indented 1 inside
  line "esac"

quoted :: String -> String
quoted s = "\"" <> s <> "\""

var :: String -> String
var s = quoted ("$" <> s)

indented :: Int -> Bash Unit -> Bash Unit
indented n = censor ((indents <> _) <<< replaceAll (Pattern "\n") (Replacement ("\n" <> indents)))
  where
    indents = joinWith "" (replicate n "  ")

line :: String -> Bash Unit
line s = tell (s <> "\n")


while :: String -> Bash Unit -> Bash Unit
while condition loop = do
  line $ "while " <> condition <> "; do"
  indented 1 $ do
     loop
  line "done"

shift :: Bash Unit
shift = line "shift"

capture :: String -> Bash Unit
capture varName = do
  line $ varName <> "=" <> var varName
  shift

subshell ::  Bash Unit -> Bash Unit
subshell script = do
  line "("
  indented 1 script
  line ")"

assign :: String -> String -> Bash Unit
assign varName val = do
  line $ varName <> "=" <> val
