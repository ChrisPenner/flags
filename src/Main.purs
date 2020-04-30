module Main where

import Bash (Bash, append, assign, caseOption, case_, echoErrLn, line, quoted, renderBash, scriptName, shift, subshell, var, while, if')
import Data.Argonaut (class DecodeJson, decodeJson, (.:), (.:?), (.!=))
import Data.Array (any, null)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String.CodeUnits as String
import Data.Yaml (parseFromYaml)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Options.Applicative (Parser, execParser, fullDesc, help, info, long, metavar, short, strOption, switch)
import Prelude (Unit, bind, discard, map, not, pure, show, unit, when, ($), (&&), (*>), (<$>), (<*>), (<>), (==), (>>=))
import Unsafe.Coerce (unsafeCoerce)

readStdIn :: Aff String
readStdIn = readTextFile UTF8 (unsafeCoerce 0 :: String)

data Options = Options
  { configFile   :: String
  , compilerMode :: Boolean
  }

optionsP :: Parser Options
optionsP = (\configFile compilerMode -> Options {configFile, compilerMode})
      <$> strOption
          ( long "config-file"
         <> short 'f'
         <> metavar "YAML-FILE"
         <> help "Path to flags config yaml" )
      <*> switch
          ( long "build"
         <> short 'b'
         <> help "Whether to be quiet" )


newtype ArgDescription = ArgDescription
  { name :: String
  , description :: String
  , multiple :: Boolean
  , required :: Boolean
  , default :: Maybe String
  }

instance decodeJsonArgDescription :: DecodeJson ArgDescription where
  decodeJson json =
    do
       obj <- decodeJson json
       name <- obj .: "name"
       description <- obj .:? "description" .!= ""
       multiple <- obj .:? "multiple" .!= false
       required <- obj .:? "required" .!= false
       default <- obj .:? "default" .!= Nothing
       pure (ArgDescription { name, description, multiple, required, default })

newtype FlagDescription = FlagDescription
  { shortName :: String
  , longName :: String
  , description :: String
  , multiple :: Boolean
  , hasArg :: Boolean
  , required :: Boolean
  , default :: Maybe String
  }

instance decodeJsonFlagDescription :: DecodeJson FlagDescription where
  decodeJson json =
    do
       obj <- decodeJson json
       longName <- obj .: "longName"
       shortName <- case obj .: "shortName" of
            Left err ->
              case String.charAt 0 longName of
                   Nothing -> Left err
                   Just c -> pure (String.singleton c)
            Right s -> s
       description <- obj .:? "description" .!= ""
       multiple <- obj .:? "multiple" .!= false
       hasArg <- obj .:? "hasArg" .!= false
       required <- obj .:? "required" .!= false
       default <- obj .:? "default" .!= Nothing
       pure (FlagDescription { shortName, longName,  description, multiple, hasArg, required , default})


newtype Command = Command
  { name :: String
  , description :: String
  , args :: Array ArgDescription
  , flags :: Array FlagDescription
  }

instance decodeJsonCommand :: DecodeJson Command where
  decodeJson json =
    do
       obj <- decodeJson json
       name <- obj .: "name"
       description <- obj .:? "description" .!= ""
       args <- obj .:? "args" .!= []
       flags <- obj .:? "flags" .!= []
       pure (Command {name, description, args, flags})

type Commands = Array Command

main :: Effect Unit
main = do
  Options {configFile, compilerMode} <- execParser $ info optionsP fullDesc
  launchAff_ do
    input <- readTextFile UTF8 configFile
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
  for_ cmds $ \(Command {flags}) -> do
    for_ flags $ \(FlagDescription {longName}) -> do
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
renderCmdSummary (Command {name, description, args, flags}) = do
  echoErrLn $ "  " <>
    joinWith " " [scriptName, name, argsToString args, flagsToString flags]

renderCmdHelp :: Command -> Bash Unit
renderCmdHelp cmd@(Command {name, description, args, flags}) = do
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
renderArgHelp (ArgDescription {name, description}) = do
  echoErrLn $ "  " <> name <> ": " <> description

renderFlagHelp :: FlagDescription -> Bash Unit
renderFlagHelp (FlagDescription {shortName, longName, description}) = do
  echoErrLn $ "  -" <> shortName <> ", --" <> longName <> ": " <> description

argsToString :: Array ArgDescription -> String
argsToString args =
  joinWith " " $ map renderArg args
  where
    renderArg (ArgDescription arg@({name, multiple})) =
      wrapArg arg $
        name <> if multiple then "..." else ""
    wrapArg {required} s =
      if required
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
    renderFlag (FlagDescription flag@({longName, shortName, hasArg})) =
      wrapFlag flag $
        "-" <> shortName <> "|" <> "--" <> longName <> if hasArg then "=<" <> longName <> ">" else ""
    wrapFlag {hasArg, required} flag =
      if (required && hasArg)
        then flag
        else wrapOptional flag

renderCmd :: Command -> Bash Unit
renderCmd cmd@(Command {name}) = do
  caseOption name $ do
     shift
     renderCmdArgsAndFlagsParser cmd
     line (name <> " " <> quoted "${_args[@]}")

renderCmdArgsAndFlagsParser :: Command -> Bash Unit
renderCmdArgsAndFlagsParser cmd@(Command {flags}) = do
  setDefaultFlags flags
  while "[[ $# -gt 0 ]]" $ do
     if' ("[[ -n " <> var "_skip_flag" <> " ]]") (captureArg *> shift *> line "continue") Nothing
     case_ (var "1") $ do
      for_ flags renderFlagCase
      skipFlagCase
      helpCase cmd
      caseOption "*" $ do
         captureArg
         shift

setDefaultFlags :: Array FlagDescription -> Bash Unit
setDefaultFlags flags = do
  for_ flags $ \(FlagDescription {longName, default}) -> do
     case default of
          Nothing -> pure unit
          Just def -> assign longName def

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
renderFlagCase (FlagDescription {longName, shortName, hasArg, multiple}) = do
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
