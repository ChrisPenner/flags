module Bash where

import Prelude

import Control.Monad.Writer (class MonadWriter, censor, tell)
import Data.Array (replicate)
import Data.Foldable (for_)
import Data.Maybe (Maybe)
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll)

{-- renderBash :: forall a m. MonadWriter String m => m a -> String --}
{-- renderBash = execWriter --}

caseOption :: forall m. MonadWriter String m => String -> m Unit -> m Unit
caseOption opt inside = do
  line (opt <> ")")
  indented 1 $ do
     inside
  line ";;"
  line ""

case_ :: forall m. MonadWriter String m => String -> m Unit -> m Unit
case_ arg inside = do
  line $ "case " <> arg <> " in"
  indented 1 inside
  line "esac"

quoted :: String -> String
quoted s = "\"" <> s <> "\""

var :: String -> String
var s = quoted ("$" <> s)

indented :: forall m. MonadWriter String m => Int -> m Unit -> m Unit
indented n = censor ((indents <> _) <<< replaceAll (Pattern "\n") (Replacement ("\n" <> indents)))
  where
    indents = joinWith "" (replicate n "  ")

line :: forall m. MonadWriter String m => String -> m Unit
line s = tell (s <> "\n")


while :: forall m. MonadWriter String m => String -> m Unit -> m Unit
while condition loop = do
  line $ "while " <> condition <> "; do"
  indented 1 $ do
     loop
  line "done"

if' :: forall m. MonadWriter String m => String -> m Unit -> Maybe (m Unit) -> m Unit
if' condition whenTrue mWhenFalse = do
  line $ "if " <> condition <> "; then"
  indented 1 whenTrue
  for_ mWhenFalse $ \whenFalse -> do
    line "else"
    indented 1 whenFalse
  line "fi"

shift :: forall m. MonadWriter String m => m Unit
shift = line "shift"

capture :: forall m. MonadWriter String m => String -> m Unit
capture varName = do
  line $ varName <> "=" <> var varName
  shift

subshell ::  forall m. MonadWriter String m => m Unit -> m Unit
subshell script = do
  line "("
  indented 1 script
  line ")"

assign :: forall m. MonadWriter String m => String -> String -> m Unit
assign varName val = do
  line $ varName <> "=" <> val

append :: forall m. MonadWriter String m => String -> String -> m Unit
append varName val = do
  line $ varName <> "+=(" <> val <> ")"

echoErrLn :: forall m. MonadWriter String m => String -> m Unit
echoErrLn s = line $ "echo \"" <> s <> "\" >&2"

scriptName :: String
scriptName = var "0"

inc :: forall m. MonadWriter String m => String -> m Unit
inc c = line $ c <> "=$((" <> c <> "+1))"

func :: forall m. MonadWriter String m => String -> m Unit -> m Unit
func name body = do
  line $ "function " <> name <> "(){"
  indented 1 body
  line $ "}"

spacer :: forall m. MonadWriter String m => m Unit
spacer = line "echo"
