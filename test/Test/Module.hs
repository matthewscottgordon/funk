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

module Test.Module (tests) where

import           Test.HUnit
import           Test.Framework (testGroup)
import qualified Test.Framework
import           Test.Framework.Providers.HUnit

import           Funk.Module
import           Funk.AST (Decl(..),Def(..),Expr(..),Type(TypeName))
import           Funk.Names

import           Control.Monad (forM_)

tests :: Test.Framework.Test
tests =
  testGroup "Module Tests" [
    testGroup "getDecl returns..." [
      testCase "...Nothing for empty Module"
               testGetDeclEmptyModule,
      testCase "...matching Decl in Module with one Decl"
               testGetDeclWithOneDecl,
      testCase "...Nothing if name doesn't match"
               testGetDeclWithWrongName,
      testCase "...matching Decls in Module with two Decls"
               testGetDeclWithTwoDecls,
      testCase "...Nothing for Module with only defs"
               testGetDeclWithOnlyDefs ],
    testGroup "getDecls returns..." [
      testCase "...empty list for empty Module"
               testGetDeclsEmptyModule,
      testCase "...one Decl for Module with one Decl"
               testGetDeclsCount,
      testCase "...matching Decl for Module with one Decl"
               testGetDeclsValue,
      testCase "...two Decls for Module with one Decl"
               testGetDeclsCount2,
      testCase "...matching Decl for Module with two Decls"
               testGetDeclsValue2,
      testCase "...empty list for Module with only defs"
               testGetDeclsWithOnlyDefs ],
    testGroup "getDefs returns..." [
      testCase "...empty list for empty Module"
               testGetDefsEmptyModule,
      testCase "...one Def for Module with one Def"
               testGetDefsCount,
      testCase "...matching Def for Module with one Def"
               testGetDefsValue,
      testCase "...two Defs for Module with two Defs"
               testGetDefsCount2,
      testCase "...matching Defs for Module with two Defs"
               testGetDefsValue2,
      testCase "...empty list for Module with only Decls"
               testGetDefsWithOnlyDecls ]]


testGetDeclEmptyModule = (getDecl empty "foo") @?= Nothing

testGetDeclWithOneDecl = (getDecl m "foo") @?= Just d
    where m = addDecl d empty
          d = Decl "foo" (TypeName "Foo")

testGetDeclWithWrongName = (getDecl m "food") @?= Nothing
    where m = addDecl d empty
          d = Decl "foo" (TypeName "Foo")

testGetDeclWithTwoDecls = do
  (getDecl m "foo") @?= Just d1
  (getDecl m "bar") @?= Just d2
    where m = foldr addDecl empty [d1,d2]
          d1 = Decl "foo" (TypeName "Foo")
          d2 = Decl "bar" (TypeName "Bar")

testGetDeclWithOnlyDefs = (getDecl m "foo") @?= Nothing
    where 
      m = addDef d empty
      d = Def "foo" [] (FloatLiteral 1)

testGetDeclsEmptyModule = length (getDecls empty) @?= 0

testGetDeclsCount = length (getDecls m) @?= 1
    where m = addDecl d empty
          d = Decl "foo" (TypeName "Foo")

testGetDeclsValue = (getDecl m "foo") @?= Just d
    where m = addDecl d empty
          d = Decl "foo" (TypeName "Foo")

testGetDeclsCount2 = length (getDecls m) @?= 2
    where m = foldr addDecl empty [d1,d2]
          d1 = Decl "foo" (TypeName "Foo")
          d2 = Decl "bar" (TypeName "Bar")

testGetDeclsValue2 = forM_ [d1,d2] $ \d ->
                     assert $ d `elem` (getDecls m)
    where m = foldr addDecl empty [d1,d2]
          d1 = Decl "foo" (TypeName "Foo")
          d2 = Decl "bar" (TypeName "Bar")

testGetDeclsWithOnlyDefs = length (getDecls m) @?= 0
    where 
      m = addDef d empty
      d = Def "foo" [] (FloatLiteral 1)

testGetDefsEmptyModule =
    getFunctions empty @=? ([] :: [Def ResolvedName])

testGetDefsCount = length (getFunctions m) @?= 1
    where 
      m = addDef d empty
      d = Def "bar" [] (FloatLiteral 1)

testGetDefsValue = assert $ d `elem` getFunctions m
    where 
      m = addDef d empty
      d = Def "foo" [] (FloatLiteral 1)

testGetDefsCount2 = length (getFunctions m) @?= 2
    where 
      m = foldr addDef empty [d1,d2]
      d1 = Def "foo" [] (FloatLiteral 1)
      d2 = Def "bar" [] (FloatLiteral 2.3)
          
testGetDefsValue2 = forM_ [d1,d2] $ \d ->
                     assert $ d `elem` (getFunctions m)
    where 
      m = foldr addDef empty [d1,d2]
      d1 = Def "foo" [] (FloatLiteral 1)
      d2 = Def "bar" [] (FloatLiteral 2.3)

testGetDefsWithOnlyDecls = length (getFunctions m) @?= 0
    where m = addDecl d empty
          d = Decl "foo" (TypeName "Foo")
                            
