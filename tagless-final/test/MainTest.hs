{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module MainTest where

import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Map as M
import Test.HUnit
import Main (findToken, TestApp(..))


type FileSystem = M.Map FilePath [String]

testFindToken :: Test
testFindToken = TestCase $ do
    let initialFS = M.fromList [("test1.txt", ["hello world", "foo bar"]), ("test2.txt", ["token here", "another line"])]
    let results = runIdentity $ evalStateT (runTestApp (findToken "." "token")) initialFS
    assertEqual "Find 'token'" [("test2.txt", 1, "token here")] results

testFindTokenInMock :: Test
testFindTokenInMock = TestCase $ do
    let initialFS = M.fromList [("file1", ["hello world", "token here"]), ("file2", ["no match", "another token line"])]
    let results = runIdentity $ evalStateT (runTestApp (findToken "." "token")) initialFS
    assertEqual "Find 'token' in mock" [("file1", 2, "token here"), ("file2", 2, "another token line")] results

testFindHello :: Test
testFindHello = TestCase $ do
    let initialFS = M.fromList [("file1", ["hello world", "token here"]), ("file2", ["no match", "another token line"])]
    let results = runIdentity $ evalStateT (runTestApp (findToken "." "hello")) initialFS
    assertEqual "Find 'hello'" [("file1", 1, "hello world")] results

testFindMissing :: Test
testFindMissing = TestCase $ do
    let initialFS = M.fromList [("file1", ["hello world", "token here"]), ("file2", ["no match", "another token line"])]
    let results = runIdentity $ evalStateT (runTestApp (findToken "." "missing")) initialFS
    assertEqual "Find 'missing'" [] results

tests :: Test
tests = TestList [
    TestLabel "testFindToken" testFindToken,
    TestLabel "testFindTokenInMock" testFindTokenInMock,
    TestLabel "testFindHello" testFindHello,
    TestLabel "testFindMissing" testFindMissing
    ]

main :: IO ()
main = do
  runTestTT tests >>= print
