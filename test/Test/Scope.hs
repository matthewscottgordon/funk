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

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Test.Scope (tests) where

import Test.HUnit (assertEqual, assertFailure)
import Test.Framework (testGroup)
import qualified Test.Framework
import Test.Framework (Test)
import Test.Framework.Providers.HUnit

import Data.List (foldl')

import Funk.Scope as Target
import Funk.Names


tests :: Test.Framework.Test
tests = testGroup "Funk.Scope Tests"  [
  storeAndFindOneName,
  storeAndFindTwoNames,
  undefinedNameIsNotFound,
  findItemInOuterScope,
  findItemInInnerScope,
  innerScopeOverridesOuter
  ]

{-data DummyScope = Scope | Global
                deriving (Show, Eq)

data DummyName = DummyName String DummyScope
               deriving(Show, Eq)

instance ScopedName DummyName DummyScope where
  name (DummyName n _) = n
  location (DummyName _ l) = l
  globalLocation = Global
  scopedName = DummyName-}


foo = UnresolvedName "foo"
foo' = ResolvedName "foo" GlobalRef

storeAndFindOneName :: Test
storeAndFindOneName = testCase "Store and find one name" $
  assertEqual
    "Wrong result"
    (findName (addNameToScope createGlobalScope foo) "foo")
    (Just foo')

bar = UnresolvedName "bar"
bar' = ResolvedName "bar" GlobalRef

storeAndFindTwoNames :: Test
storeAndFindTwoNames = testCase "Store and find two names" $ do
  let td = foldl' addNameToScope createGlobalScope [foo, bar]
  assertEqual
    "Wrong first result"
    (findName td "foo")
    (Just foo')
  assertEqual
    "Wrong secoond result"
    (findName td "bar")
    (Just bar')

baz = UnresolvedName "baz"

undefinedNameIsNotFound :: Test
undefinedNameIsNotFound = testCase "Undefined name is not found" $
  let td = foldl' addNameToScope createGlobalScope [foo, bar]
      expected = Nothing
  in assertEqual "Found name" (findName td "baz") expected
     
qux = UnresolvedName "qux"
quux = UnresolvedName "quux"

findItemInOuterScope :: Test
findItemInOuterScope = testCase "Find item in outer scope" $
  let td' = foldl' addNameToScope createGlobalScope [foo, bar]
      td  = foldl' addNameToScope
                   (createScope td' (FunctionParamRef "func"))
                   [baz, qux, quux]
  in assertEqual
     "Wrong result"
     (findName td "bar")
     (Just bar')
     
qux' = ResolvedName "qux" (FunctionParamRef "func")

findItemInInnerScope :: Test
findItemInInnerScope = testCase "Find item in inner scope" $
  let td' = foldl' addNameToScope createGlobalScope [foo, bar]
      td  = foldl' addNameToScope
                   (createScope td' (FunctionParamRef "func"))
                   [baz, qux, quux]
  in assertEqual
     "Wrong result"
     (findName td "qux")
     (Just qux')

bar2 = UnresolvedName "bar"
bar2' = ResolvedName "bar" (FunctionParamRef "func")

innerScopeOverridesOuter :: Test
innerScopeOverridesOuter = testCase "Inner scope overrides outer" $
  let td' = foldl' addNameToScope createGlobalScope [foo, bar]
      td = foldl' addNameToScope
                  (createScope td' (FunctionParamRef "func"))
                  [foo, bar2]
  in assertEqual
     "Wrong result"
     (findName td "bar")
     (Just bar2')
