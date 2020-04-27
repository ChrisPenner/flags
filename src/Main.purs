module Main where

import Prelude

import Control.Monad.Writer (Writer, censor, execWriter, tell)
import Data.Argonaut (decodeJson, jsonParser)
import Data.Array (replicate)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Unsafe.Coerce (unsafeCoerce)
import Bash

readStdIn :: Aff String
readStdIn = readTextFile UTF8 (unsafeCoerce 0 :: String)

sampleJsonString :: String
sampleJsonString = """
[
   { "name": "add"
   , "description": "Add a todo to the list"
   , "args": [
      { "name": "todo"
      , "description": "The todo you'd like to add"
      , "acceptMultiple": true
      }
    ],
    "flags": []
  },
  { "name": "list"
  , "description": "List out your existing TODOs"
  , "args": []
  , "flags": 
    [ { "longName": "reverse"
      , "shortName": "r"
      , "description" : "Reverse the TODO list"
      , "acceptMultiple": false
      },
      { "longName": "query"
      , "shortName": "q"
      , "description": "List only TODOs containing this text"
      , "acceptMultiple": false
      }
    ]
  }
]
"""


type ArgDescription =
  { name :: String
  , description :: String
  , acceptMultiple :: Boolean
  }

type FlagDescription =
  { shortName :: String
  , longName :: String
  , description :: String
  , acceptMultiple :: Boolean
  }


type Command =
  { name :: String
  , description :: String
  , args :: Array ArgDescription
  , flags :: Array FlagDescription
  }
type Commands = Array Command


main :: Effect Unit
main = launchAff_ do
  {-- input <- readStdIn --}
  let input = sampleJsonString
  case jsonParser input >>= decodeJson of
       Left err -> Console.log $ show err
       Right (obj :: Commands) -> do
          let bash = execWriter $ toBash obj
          Console.log $ bash

toBash :: Commands -> Bash Unit
toBash cmds = subshell $ do
  line "local _args=()"
  initCommandsVars cmds
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
  pure unit

renderCmd :: Command -> Bash Unit
renderCmd cmd = do
  caseOption cmd.name $ do
     shift
     renderCmdArgsAndFlagsParser cmd
     line (cmd.name <> " " <> quoted "${_args[@]}")


renderCmdArgsAndFlagsParser :: Command -> Bash Unit
renderCmdArgsAndFlagsParser {flags} = do
  while "[[ #$ -gt 0 ]]" $ do
     case_ (var "1") $ do
      for_ flags renderFlagCase
      caseOption "*" $ do
         captureArg
         shift

captureArg :: Bash Unit
captureArg = do
  line $ "_args+=(" <> (var "1") <> ")"


renderFlagCase :: FlagDescription -> Bash Unit
renderFlagCase {longName, shortName} = do
  caseOption (joinWith "" ["--", longName, "|", "-", shortName ]) $ do
    line $ longName <> "=" <> (var "1")
    shift
