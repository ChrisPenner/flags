module Bash where

import Prelude

import Control.Monad.Writer (Writer, censor, execWriter, tell)
import Data.Array (replicate)
import Data.Foldable (for_)
import Data.Maybe (Maybe)
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll)

type Bash a = Writer String a

renderBash :: forall a. Bash a -> String
renderBash = execWriter

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

if' :: String -> Bash Unit -> Maybe (Bash Unit) -> Bash Unit
if' condition whenTrue mWhenFalse = do
  line $ "if " <> condition <> "; then"
  indented 1 whenTrue
  for_ mWhenFalse $ \whenFalse -> do
    line "else"
    indented 1 whenFalse
  line "fi"

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

append :: String -> String -> Bash Unit
append varName val = do
  line $ varName <> "+=(" <> val <> ")"

echoErrLn :: String -> Bash Unit
echoErrLn s = line $ "echo \"" <> s <> "\" >&2"

scriptName :: String
scriptName = var "0"

inc :: String -> Bash Unit
inc c = line $ c <> "=$((" <> c <> "+1))"
