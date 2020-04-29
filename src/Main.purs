module Main where

import Bash (Bash, append, assign, caseOption, case_, echoErrLn, line, quoted, renderBash, scriptName, shift, subshell, var, while, if')
import Control.Alt (map)
import Control.Alternative (pure, when)
import Data.Argonaut (decodeJson)
import Data.Array (any, drop, null)
import Data.BooleanAlgebra (not)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Unit (unit)
import Data.Yaml (parseFromYaml)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Process (argv, exit)
import Prelude ((==), (*>), (&&), Unit, bind, discard, show, ($), (<>), (>>=))
import Unsafe.Coerce (unsafeCoerce)

readStdIn :: Aff String
readStdIn = readTextFile UTF8 (unsafeCoerce 0 :: String)

type ArgDescription =
  { name :: String
  , description :: String
  , multiple :: Boolean
  , validators :: Array String
  }

type FlagDescription =
  { shortName :: String
  , longName :: String
  , description :: String
  , multiple :: Boolean
  , hasArg :: Boolean
  , validators :: Array String
  }

type Command =
  { name :: String
  , description :: String
  , args :: Array ArgDescription
  , flags :: Array FlagDescription
  }
type Commands = Array Command

main :: Effect Unit
main = do
  args <- argv
  -- Drop node location and script location
  configFile <- case (drop 2 args) of
      [] -> pure Nothing
      [f] -> pure (Just f)
      _ -> do
         Console.log $ "Usage:"
         Console.log $ "  flags <config-file.yaml>"
         Console.log $ "    Generate a bash script from a yaml command config"
         Console.log $ "  flags -"
         Console.log $ "    Generate a bash script from yaml provided via stdin"
         exit 1
  launchAff_ do
    input <- case configFile of
         Nothing -> readStdIn
         Just f -> readTextFile UTF8 f
    case parseFromYaml input >>= decodeJson of
         Left err -> Console.log $ show err
         Right (obj :: Commands) -> do
            let bash = renderBash $ toBash obj
            Console.log $ bash

toBash :: Commands -> Bash Unit
toBash cmds = do
  line "#!/bin/bash"
  subshell $ do
    line "_args=()"
    case_ (var "1") $ do
      for_ cmds renderCmd
      defaultSubcommand cmds

initCommandsVars :: Commands -> Bash Unit
initCommandsVars cmds = do
  for_ cmds $ \{flags} -> do
    for_ flags $ \{longName} -> do
      line $ "local " <> longName

defaultSubcommand :: Commands -> Bash Unit
defaultSubcommand cmds = do
  caseOption "*" $ do
    shift
    renderTopLevelHelp cmds

renderTopLevelHelp :: Commands -> Bash Unit
renderTopLevelHelp cmds = do
  echoErrLn "Usage:"
  echoErrLn $ "  " <> scriptName <> " <command>"
  echoErrLn ""
  echoErrLn "More info:"
  echoErrLn $ "  " <> scriptName <> " [command] --help"
  echoErrLn ""
  echoErrLn $ "Commands:"
  for_ cmds renderCmdSummary

renderCmdSummary :: Command -> Bash Unit
renderCmdSummary {name, description, args, flags} = do
  echoErrLn $ "  " <>
    joinWith " " [scriptName, name, argsToString args, flagsToString flags]

renderCmdHelp :: Command -> Bash Unit
renderCmdHelp cmd@{name, description, args, flags} = do
  echoErrLn "Usage:"
  renderCmdSummary cmd
  echoErrLn ""
  when (not (null args)) $ do
    echoErrLn "Args: "
    for_ args renderArgHelp 
  when (not (null flags)) $ do
     echoErrLn "Flags: "
     for_ flags renderFlagHelp

renderArgHelp :: ArgDescription -> Bash Unit
renderArgHelp {name, description} = do
  echoErrLn $ "  " <> name <> ": " <> description

renderFlagHelp :: FlagDescription -> Bash Unit
renderFlagHelp {shortName, longName, description} = do
  echoErrLn $ "  -" <> shortName <> ", --" <> longName <> ": " <> description

argsToString :: Array ArgDescription -> String
argsToString args =
  joinWith " " $ map renderArg args
  where
    renderArg arg@({name, multiple}) =
      wrapArg arg $
        name <> if multiple then "..." else ""
    wrapArg {validators} s =
      if isRequired validators
        then s
        else wrapOptional s

wrapOptional :: String -> String
wrapOptional s =  "[" <> s <> "]"

isRequired :: Array String -> Boolean
isRequired validators =
  any (_ == "required") validators

flagsToString :: Array FlagDescription -> String
flagsToString flags =
  joinWith " " $ map renderFlag flags
  where
    renderFlag flag@({longName, shortName, hasArg}) =
      wrapFlag flag $
        "-" <> shortName <> "|" <> "--" <> longName <> if hasArg then "=<" <> longName <> ">" else ""
    wrapFlag {hasArg, validators} flag =
      if (isRequired validators && hasArg)
        then flag
        else wrapOptional flag

renderCmd :: Command -> Bash Unit
renderCmd cmd = do
  caseOption cmd.name $ do
     shift
     renderCmdArgsAndFlagsParser cmd
     line (cmd.name <> " " <> quoted "${_args[@]}")

renderCmdArgsAndFlagsParser :: Command -> Bash Unit
renderCmdArgsAndFlagsParser cmd@{flags} = do
  while "[[ $# -gt 0 ]]" $ do
     if' ("[[ -n " <> var "_skip_flag" <> " ]]") (captureArg *> shift *> line "continue") Nothing
     case_ (var "1") $ do
      for_ flags renderFlagCase
      skipFlagCase
      helpCase cmd
      caseOption "*" $ do
         captureArg
         shift

captureArg :: Bash Unit
captureArg = do
  line $ "_args+=(" <> (var "1") <> ")"

skipFlagCase :: Bash Unit
skipFlagCase = do
  caseOption "\"--\"" $ do
    shift
    line "_skip_flag=true"

helpCase :: Command -> Bash Unit
helpCase cmd = do
  caseOption "-h|--help" $ do
    renderCmdHelp cmd
    line "exit 1"

renderFlagCase :: FlagDescription -> Bash Unit
renderFlagCase {longName, shortName, hasArg, multiple} = do
  caseOption (joinWith "" ["--", longName, "|", "-", shortName ]) $ do
    shift
    if hasArg
      then do
         if multiple
          then append longName (var "1")
          else assign longName (var "1")
         shift
      else do
         assign longName "true"
