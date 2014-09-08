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

import qualified Funk.Options as Opt
import Funk.Options (parseOpts, Options(..))
import Funk.Names
import Funk.AST as AST
import Funk.Module as Module
import Funk.Parser
import Funk.Renamer
import Funk.CodeGen

import Control.Monad.Error
import Control.Applicative ((<$>))
import Control.Monad (forM, forM_)

import System.Environment (getArgs)
import System.IO ( IOMode(ReadMode),
                   Handle,
                   hClose,
                   hGetContents,
                   openFile,
                   putStrLn )

main :: IO ()
main = do
  r <- runErrorT $ do
    opts <- ErrorT (parseOpts <$> liftIO getArgs)
    main' opts
  case r of
    Left e  -> putStrLn e
    Right _ -> return ()


main' :: Options -> ErrorT String IO ()

main' (Options _ Opt.PrintVersion) = liftIO $ putStrLn "Funk prototype"

main' (Options files (Opt.Assembly outFile)) = do
  asts <- forM files $ \file -> do
    h <- openSource file
    text <- liftIO $ hGetContents h
    compile text
  forM_ asts $ \ast -> do
    llvmIR <- Funk.CodeGen.showLLVM ast
    liftIO $ putStrLn llvmIR

main' (Options _ (Opt.Object _)) = liftIO $ putStrLn "Object"

main' (Options _ (Opt.Executable _)) = liftIO $ putStrLn "Executable"


openSource :: Opt.Input -> ErrorT String IO Handle
openSource (Opt.Source filename) = liftIO $ openFile filename ReadMode

compile :: MonadError String m => String -> m (Module.Module ResolvedName)
compile input =
  Funk.Parser.parse "<stdin>" input >>= Funk.Renamer.rename >>= Funk.Renamer.collect

