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

import Test.Framework (defaultMain, Test)

import qualified Test.Lexer
import qualified Test.Parser
import qualified Test.Scope
import qualified Test.Module

tests :: [Test]
tests = [Test.Lexer.tests,
         Test.Parser.tests,
         Test.Scope.tests,
         Test.Module.tests]

main :: IO ()
main = defaultMain tests
