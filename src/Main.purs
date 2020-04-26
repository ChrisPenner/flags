module Main where

import Prelude

import Data.Argonaut (jsonParser, stringify)
import Data.Either (Either(..))
import Data.Show (show)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.Process as Process
import Node.Stream (onDataString, onEnd, onError)

readStdIn :: Aff String
readStdIn = makeAff \handler -> do
  buffer <- Ref.new ""
  onDataString Process.stdin UTF8 \chunk -> Ref.modify_ (_ <> chunk) buffer
  onEnd Process.stdin (handler =<< Right <$> Ref.read buffer)
  onError Process.stdin (handler <<< Left)
  pure nonCanceler

main :: Effect Unit
main = launchAff_ do
  input <- readStdIn
  case jsonParser input of
       Left err -> Console.log err
       Right json -> Console.log $ stringify json
