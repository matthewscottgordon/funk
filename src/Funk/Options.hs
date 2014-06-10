{-  Copyright 2013 Matthew Gordon.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express orimplied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Funk.Options (
  parseOpts,
  Options(..),
  Output(..),
  Input(..),
  getInputFiles
  ) where

import qualified System.Console.GetOpt as G
import Data.List (intercalate)

data Output = Assembly (Maybe FilePath)
            | Object (Maybe FilePath)
            | Executable (Maybe FilePath)
            | PrintVersion

data Input = Source FilePath

getInputFiles :: Options -> [FilePath]
getInputFiles (Options fs _) = map (\f -> case f of Source f' -> f') fs

data Options = Options [Input] Output

data Flag = Assemble
          | Compile
          | Help
          | Version
          | OutputFile FilePath
          deriving Eq

optDescriptions :: [G.OptDescr Flag]
optDescriptions = [
  G.Option "" ["version"] (G.NoArg Version)
  "print version information",
  G.Option "h" ["help"] (G.NoArg Help)
  "print this help message",
  G.Option "S" ["compile-only"] (G.NoArg Compile)
  "compile only, do not assemble or link",
  G.Option "c" ["compile-and-assemble"] (G.NoArg Assemble)
  "compile and assemble, do not link",
  G.Option "o" ["output"] (G.ReqArg OutputFile "<file>")
  "write output to <file>" ]

data GetOptResult = ParsedOptions [Flag] [String]
                  | BadOptions String
                  | HelpOption

getOpt :: [String] -> GetOptResult
getOpt = untuple . G.getOpt G.RequireOrder optDescriptions
  where untuple (recognized, files, unrecognized)
          | length unrecognized > 0 =
            BadOptions (unrecognizedMsg unrecognized)
          | elem Help recognized =
            HelpOption
          | (elem Compile recognized) && (elem Assemble recognized) =
            BadOptions "Cannot specify both \"-S\" and \"-c\"."
          | otherwise =
            ParsedOptions recognized files
        unrecognizedMsg (s:[]) = "Unrecognized option: \"" ++ s ++ "\"."
        unrecognizedMsg ss =
          "Unrecognized options: \"" ++
          (intercalate "\", \"" ss) ++ "\"."

mkOutput :: [Flag] -> Output
mkOutput flags | elem Version flags = PrintVersion
               | elem Assemble flags = Assembly (getOutFile flags)
               | elem Compile flags = Object (getOutFile flags)
               | otherwise = Executable (getOutFile flags)
  where getOutFile ((OutputFile s):_) = Just s
        getOutFile (_:rest)           = getOutFile rest
        getOutFile []                 = Nothing



parseOpts :: [String] -> Either String Options
parseOpts args = case getOpt args of
  BadOptions s -> Left $ G.usageInfo s optDescriptions
  HelpOption   -> Left $ G.usageInfo "header" optDescriptions
  ParsedOptions flags files ->
    Right $ Options (map Source files) (mkOutput flags)

