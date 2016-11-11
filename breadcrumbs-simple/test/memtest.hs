-- This checks that a naive list-based implementation will go out
-- of memory, while less naive ones won't.
--
-- Because heap overflow can't be caught, we run the body of the
-- test in separate processes.
--
-- This should be run with a bounded heap size (via +RTS -M[size]).
-- See options in *.cabal.

import Data.Foldable
import System.Environment
import System.Exit
import System.IO.Error
import System.Process

import qualified Data.Breadcrumbs as B
import qualified Data.Breadcrumbs.Internal as BI
import qualified Data.Breadcrumbs.List as L
import qualified Data.Breadcrumbs.DropList as D
import qualified Data.Breadcrumbs.Deque as Q
import qualified Data.Breadcrumbs.Seq as S
import Data.Breadcrumbs.Test

size = 10
iters = 100000

memtest :: (Int -> as) -> (Int -> as -> as) -> (as -> int) -> int
memtest = appendStuff size iters

tests :: [(String, Bool, Int)]
tests =
  [ ("Unit", True, memtest (\_ -> ()) (const id) (const 0))
  , ("TreeBuffer", True, memtest B.empty B.push b_len)
  , ("DropList", True, memtest D.empty D.push D.len)
  , ("Deque", True, memtest Q.empty Q.push Q.len)
  , ("Seq", True, memtest S.empty S.push S.len)
  , ("List", False, memtest L.empty L.push L.len)
  ]

b_len (B.Buffer b) = length (BI.bufferLazy b)

main = do
  args <- getArgs
  case args of
    [] -> do
      path <- getExecutablePath
      for_ tests $ \(testName, shouldSucceed, _) -> do
        putStrLn testName
        (e, s, _) <- readProcessWithExitCode path [testName] ""
        case e of
          ExitSuccess
            | shouldSucceed ->
                putStrLn $ "  OK SUCCESS\n  " ++ s
            | otherwise -> do
                putStrLn "  Should not have succeeded."
                exitFailure
          ExitFailure code
            | shouldSucceed -> do
                putStrLn "  Should not have failed."
                exitFailure
            | otherwise ->
                putStrLn $ "  OK FAILURE: " ++ show code
    testName : _
      | [(_, _, v)] <- findName -> print v
      where
        findName = filter (\(name,_,_) -> name == testName) tests
    args -> die $ "invalid arguments: " ++ show (unwords args)
