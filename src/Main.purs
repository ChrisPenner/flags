module Main where

import Prelude

import Data.Argonaut (decodeJson, jsonParser)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe)
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console as Console
import Foreign.Object (Object)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Text.Parsing.StringParser (Parser(..))
import Text.Parsing.StringParser.CodePoints as P
import Text.Parsing.StringParser.Combinators as P
import Unsafe.Coerce (unsafeCoerce)

readStdIn :: Aff String
readStdIn = readTextFile UTF8 (unsafeCoerce 0 :: String)

sampleJsonString :: String
sampleJsonString = """
{
  "add": {
    "description": "Add a todo to the list",
    "args": [
      { "name": "todo"
      , "description": "The todo you'd like to add"
      , "acceptMultiple": true
      }
    ],
    "flags": []
  },
  "list":
    { "description": "List out your existing TODOs"
    , "args": []
    , "flags": [
    { "longName": "reverse"
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
}
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
  { description :: String
  , args :: Array ArgDescription
  , flags :: Array FlagDescription
  }
type FlagsConfig = Object Command


main :: Effect Unit
main = launchAff_ do
  {-- input <- readStdIn --}
  let input = sampleJsonString
  case jsonParser input >>= decodeJson of
       Left err -> Console.log $ show err
       Right (obj :: Object Command) -> do
          Console.log <<< show $ obj

{-- toBash :: Command -> String --}
{-- toBash {template} = "" --}
