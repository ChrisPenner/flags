module Main where


import Bash (Bash, append, assign, caseOption, case_, echoErrLn, line, quoted, renderBash, scriptName, shift, subshell, var, while, if')
import Data.Argonaut (class DecodeJson, decodeJson, (.:), (.:?), (.!=))
import Data.Array (any, fromFoldable, null)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe, maybe, optional)
import Data.String (joinWith, Pattern(..), Replacement(..), toLower, replace)
import Data.String.CodeUnits as String
import Data.Yaml (parseFromYaml)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.ChildProcess (StdIOBehaviour(..), defaultExecSyncOptions, execFileSync)
import Node.Encoding (Encoding(..))
import Node.FS (FileDescriptor)
import Node.FS.Aff (exists, readTextFile, writeTextFile)
import Node.Path (FilePath, dirname, sep)
import Node.Process (exit)
import Options.Applicative (Parser, ParserInfo, argument, command, execParser, fullDesc, help, helper, hsubparser, info, long, many, metavar, progDesc, short, str, strArgument, strOption, (<**>))
import Prelude (Unit, bind, discard, map, not, pure, show, unit, void, when, ($), (&&), (*>), (<$>), (<*>), (<<<), (>>>), (<>), (==), (>>=))
import Unsafe.Coerce (unsafeCoerce)

readStdIn :: Aff String
readStdIn = readTextFile UTF8 (unsafeCoerce 0 :: String)

data Choice =
    Run {configFile :: Maybe String, srcFile :: String, passthroughArgs :: List String}
  | Build {configFile :: Maybe String, srcFile :: Maybe String, outputFile :: Maybe String}
  | Init

runOptionsP :: ParserInfo Choice
runOptionsP =
  info p (fullDesc <> progDesc "Parse arguments and flags provided after -- and run the provided source file against them.")
    where
      p =
        (\configFile srcFile passthroughArgs -> Run {configFile, srcFile, passthroughArgs})
          <$> optional configFileP
          <*> (srcFileP false)
          <*> argP


buildOptionsP :: ParserInfo Choice
buildOptionsP =
  info p (fullDesc <> progDesc "Compile argument handling logic")
    where
      p =
        (\configFile srcFile outputFile -> Build {configFile, srcFile, outputFile})
        <$> optional configFileP
        <*> optional (srcFileP true)
        <*> optional outFileP

initOptionsP :: ParserInfo Choice
initOptionsP =
  info (pure Init) (fullDesc <> progDesc "Initialize a flags.yaml in the current directory")

configFileP :: Parser String
configFileP =
  strOption
          ( long "config-file"
         <> short 'f'
         <> metavar "YAML-FILE"
         <> help "Path to yaml file containing your flags config. Defaults to 'flags.yaml' in the srcFile directory" )

srcFileP :: Boolean -> Parser String
srcFileP opt =
  strArgument
  ( metavar "SOURCE-FILE"
  <> help ("""Path to your bash script.""" <> if opt then "If omitted, only output flag parsing logic" else "" ))

outFileP :: Parser String
outFileP =
  strOption
  ( long "out-file"
  <> short 'o'
  <> metavar "OUT-FILE"
  <> help "Path to write compiled result" )

argP :: Parser (List String)
argP = many (argument str (metavar "-- <script args>..."
                           <> help "Arguments following -- will be parsed and handed off to your script"
  ))

data ArgType =
    File
  | Dir
  | Str
  | Number

instance decodeJsonArgType :: DecodeJson ArgType where
  decodeJson json = do
       typeString <- decodeJson json
       case typeString of
            "string" -> pure Str
            "number" -> pure Number
            "file" -> pure File
            "dir" -> pure Dir
            _ -> Left $ "Unknown arg type: " <> typeString


newtype ArgDescription = ArgDescription
  { name :: String
  , description :: String
  , multiple :: Boolean
  , required :: Boolean
  , default :: Maybe String
  , typ :: ArgType
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
       typ <- obj .:? "type" .!= Str
       pure (ArgDescription { name, description, multiple, required, default, typ })

newtype FlagDescription = FlagDescription
  { shortName :: String
  , longName :: String
  , description :: String
  , multiple :: Boolean
  , hasArg :: Boolean
  , required :: Boolean
  , default :: Maybe String
  , typ :: ArgType
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
       typ <- obj .:? "type" .!= Str
       pure (FlagDescription { shortName, longName,  description, multiple, hasArg, required , default, typ})


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

readConfigFile :: {srcFilePath :: Maybe FilePath, configFilePath :: Maybe FilePath} ->  Aff Commands
readConfigFile {srcFilePath, configFilePath} = 
  parseYamlConfig actualConfigPath
  where
    actualConfigPath = fromMaybe defaultConfigPath configFilePath
    defaultConfigPath = maybe "." dirname srcFilePath <> sep <> "flags.yaml"

runBuild :: {configFile :: Maybe String, srcFile :: Maybe String, outputFile :: Maybe String} -> Aff Unit
runBuild {configFile, outputFile, srcFile} = do
  conf <- readConfigFile ({srcFilePath: srcFile, configFilePath: configFile})
  let bash = renderBash $ toBash conf
  totalOutput <- case srcFile of
    Nothing -> pure bash
    Just f -> do
         src <- readTextFile UTF8 f
         pure (joinWith "\n" ["#!/bin/bash", src, bash])
  case outputFile of
    Nothing -> Console.log totalOutput
    Just f ->
      writeTextFile UTF8 f totalOutput

parseYamlConfig :: FilePath -> Aff Commands
parseYamlConfig configFile = do
  input <- readTextFile UTF8 configFile
  case parseFromYaml input >>= decodeJson of
         Left err -> do
            Console.error $ show err
            liftEffect $ exit 1
         Right (obj :: Commands) -> pure obj


runRun :: {configFile :: Maybe String, srcFile :: String, passthroughArgs :: List String} -> Aff Unit
runRun {configFile, srcFile, passthroughArgs} = do
  conf <- readConfigFile ({srcFilePath: Just srcFile, configFilePath: configFile})
  let bash = renderBash $ toBash conf
  src <- readTextFile UTF8 srcFile
  let totalOutput = (joinWith "\n" [src, bash])
  void <<< liftEffect $ execFileSync "/bin/bash" (["-c", totalOutput, srcFile] <> fromFoldable passthroughArgs) (defaultExecSyncOptions{stdio=proxyPipes})

runInit :: Aff Unit
runInit = do
  existingConfig <- exists "./flags.yaml"
  if existingConfig then do
    Console.error "Refusing to overwrite existing flags.yaml"
    liftEffect $ exit 1
                    else do
    writeTextFile UTF8 "./flags.yaml" initYaml

initYaml :: String
initYaml = """# List of subcommands
- name: command-name
  # This description is printed in the help message
  description: "This is a command"
  # Argument configuration
  args:
      # The name of a positional argument
    - name: positional-argument
      # This description is printed in the help message
      description: "A positional argument"
      # (default: false) Whether multiple values can be provided for this argument
      multiple: false
      # (default: false) Whether the argument is required or optional
      required: false
      # (default: null) A default value for optional arguments
      default: null
  flags:
      # (default: first char of long-name)
    - shortName: "f"
      # (required) Both the longname of the flag, and the name of the environment variable which it will be bound to.
      # dashes will be replaced with underscores in variable names
      longName: "flag"
      # This description is printed in the help message
      description: "A flag option"
      # (default: false) Whether the flag can be provided multiple times
      multiple: false
      # (default: false) Whether the flag takes an argument
      # Variables for flags without arguments will be unset by default
      # and "true" if the arg is provided.
      hasArg: false
"""

proxyPipes :: Array (Maybe StdIOBehaviour)
proxyPipes = [ Just (ShareFD (unsafeCoerce 0 :: FileDescriptor))
             , Just (ShareFD (unsafeCoerce 1 :: FileDescriptor))
             , Just (ShareFD (unsafeCoerce 2 :: FileDescriptor))
             ]



fullP :: ParserInfo Choice
fullP = info (p <**> helper) fullDesc
  where 
    p = hsubparser $
          command "build" (buildOptionsP)
          <> command "run" runOptionsP
          <> command "init" initOptionsP

main :: Effect Unit
main = do
  choice <- execParser fullP
  launchAff_ do
    case choice of
         Build buildData -> runBuild buildData
         Run runData -> runRun runData
         Init -> runInit

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

flagToVar :: String -> String
flagToVar = toLower >>> replace (Pattern "-") (Replacement "_")
