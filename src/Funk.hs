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

{-# LANGUAGE FlexibleContexts #-}

import Funk.Options
import Funk.Names
import Funk.AST
import Funk.Parser
import Funk.Renamer
import Funk.CodeGen

import Control.Monad.Error


main :: IO ()
main = do
  r <- runErrorT main'
  case r of
    Left e  -> putStrLn ("ERROR: " ++ e)
    Right _ -> return ()

main' :: ErrorT String IO ()
main' = do
  input <- liftIO getContents
  ast <- compile input
  llvmIR <- Funk.CodeGen.showLLVM ast
  liftIO $ putStrLn llvmIR
  return ()


compile :: MonadError String m => String -> m (Module (ResolvedName ()))
compile input =
  Funk.Parser.parse "<stdin>" input >>= Funk.Renamer.rename

