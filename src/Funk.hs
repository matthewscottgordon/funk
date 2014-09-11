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
import qualified Funk.AST as AST
import Funk.Module
import Funk.Parser
import Funk.Renamer
import Funk.CodeGen

import Control.Monad.Error
import Control.Applicative ((<$>))
import Control.Monad (forM, forM_)

import System.Environment (getArgs)
import System.IO ( IOMode(ReadMode,WriteMode),
                   Handle,
                   hClose,
                   hGetContents,
                   openFile,
                   putStrLn,
                   hPutStr,
                   stdout)

-- Parse options and pass then call main', then print error message
-- if there were errors.
main :: IO ()
main = do
  r <- runErrorT $ do
    opts <- ErrorT (parseOpts <$> liftIO getArgs)
    main' opts
  case r of
    Left e  -> putStrLn e
    Right _ -> return ()


-- The real main function. Takes the Options structure created by
-- main does the actual work.
main' :: Options -> ErrorT String IO ()

-- Print version and exit
main' (Options _ Opt.PrintVersion) = liftIO $ putStrLn "Funk prototype"

-- Compile all input files and output LLVM IR
main' (Options files (Opt.Assembly outFile)) = do
  modules <- forM files compileFile
  withDest outFile $ \hOut ->
      forM_ modules $ \m -> do
        llvmIR <- Funk.CodeGen.showLLVM m
        liftIO $ hPutStr hOut llvmIR

-- Compile all input files and produce a binary object file.
main' (Options _ (Opt.Object _)) = liftIO $ putStrLn "Object"

-- Compile all input files and produce an executable.
main' (Options _ (Opt.Executable _)) = liftIO $ putStrLn "Executable"


compileFile :: (MonadIO m, MonadError String m) =>
               Opt.Input -> m (Module ResolvedName)
compileFile sourceName@(Opt.Source filename) =
    withFile filename ReadMode $ 
                 compile sourceName <=< (liftIO . hGetContents)


compile :: MonadError String m =>
           Opt.Input -> String -> m (Module ResolvedName)
compile (Opt.Source fileName) input =
  Funk.Parser.parse fileName input >>=
  Funk.Renamer.rename >>=
  Funk.Renamer.collect


withFile :: (MonadIO m, MonadError String m) =>
            FilePath -> IOMode -> (Handle -> m a) -> m a
withFile filePath mode f = do
  h <- liftIO $openFile filePath mode
  r <- f h
  liftIO $ hClose h
  return r


withDest :: (MonadIO m, MonadError String m) =>
            Maybe FilePath -> (Handle -> m a) -> m a
withDest Nothing f = f stdout
withDest (Just filePath) f = withFile filePath WriteMode f
