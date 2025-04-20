module Main where

import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Qrisc32Model

main :: IO ()
main = do
  args <- getArgs
  case args of
    [name] ->
      case vectorSpec name of
        Nothing -> usage
        Just spec -> do
          memory <- runVector spec
          let failures =
                [ (idx, got, expected)
                | (idx, expected) <- expectedDmem spec
                , let got = Map.findWithDefault 0 idx memory
                , got /= expected
                ]
          case failures of
            [] -> putStrLn (passLine spec)
            (idx, got, expected):_ -> do
              putStrLn $ "FAIL: dmem[" ++ show idx ++ "]=" ++ show got ++ " expected " ++ show expected
              exitFailure
    _ -> usage

usage :: IO ()
usage = do
  putStrLn "usage: runghc -isrc tb/Main.hs smoke|flags|isa|axi"
  exitFailure
