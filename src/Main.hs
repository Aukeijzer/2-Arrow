{-# LANGUAGE BlockArguments #-}
module Main where

import qualified Data.Map as L
import Data.Char
import ParseLib.Abstract

import Algebra
import Model
import Interpreter
import Lexer
import Parser

-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive enviroment state@(ArrowState space pos heading stack) = do 
  putStrLn $ show stack
  putStrLn $ show heading ++ " " ++ show pos
  putStrLn $ printSpace space
  let continue = case step enviroment state of
        Done newSpace newPos newHeading -> do 
          putStrLn $ printSpace space
          putStrLn "Program finished"
        Ok newState -> interactive enviroment newState
        Fail s -> putStrLn $ "Program failed with error: " ++ s
  let askInput = do
        putStrLn "[C]ontinue or [S]top?"
        chars <- getLine
        case map toUpper chars of
          "C" -> continue
          "CONTINUE" -> continue
          "S" -> return ()
          "STOP" -> return ()
          _ -> askInput
  askInput
batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch enviroment state@(ArrowState space pos heading stack) = case step enviroment state of
  Ok state -> batch enviroment state
  Done x y z -> (x,y,z)
  Fail s -> (L.fromList [],(0,0),(North))

-- Main function.
main :: IO ()
main = do
  --If the wrong file is given the program just throws and error and quits.
  --This is done instead of asking for a new input, because this usually means something is wrong with the file
  --Also this is the first thing a user does, they can just run the program again.
  putStrLn "Give program file (file must be in directory programs): "
  programFile <- getLine
  programChars <- readFile $ "programs/" ++ programFile ++ ".arrow"
  let environment = toEnvironment programChars
  putStrLn "Input program:"
  putStrLn ""
  putStrLn programChars

  putStrLn "Give space file (file must be in directory spaces)"
  spaceFile <- getLine
  spaceChars <- readFile $ "spaces/" ++ spaceFile ++ ".space"
  let parsedSpace = parse parseSpace spaceChars
  let space = case parsedSpace of 
        [] -> error "Space file invalid syntax"
        (x:xs) -> fst x
  putStrLn "Input space:"
  putStrLn ""
  putStrLn spaceChars

  let askPos = do
        putStrLn "Give Position arrow e.g. 6 4"
        chars <- getLine
        let pos = words chars
        if length pos == 2 && all (all isDigit) pos
        then return (read $ pos !! 0, read $ pos !! 1)
        else askPos
  pos <- askPos

  let askHeading = do
        putStrLn "Choose heading: [N]orth, [E]ast, [S]outh, [W]est"
        chars <- getLine
        case map toUpper chars of
          "N" -> return North
          "NORTH" -> return North
          "E" -> return East
          "EAST" -> return East
          "S" -> return South
          "SOUTH" -> return South
          "W" -> return West
          "WEST" -> return West
  heading <- askHeading
  putStrLn $ show ((\(Just x) -> x) $ L.lookup "start" environment)
  let state = ArrowState space pos heading ((\(Just x) -> x) $ L.lookup "start" environment)
  
  let askMode = do
        putStrLn "Run in [I]nteractive or [B]atch?"
        chars <- getLine
        case map toUpper chars of
          "I" -> return interactive
          "INTERACTIVE" -> return interactive
          "B" -> return printBatch
          "BATCH" -> return printBatch
          _ -> askMode
  mode <- askMode

  mode environment state

printBatch :: Environment -> ArrowState -> IO ()
printBatch environment state = 
  putStrLn $ printSpace $ (\(x,_,_) -> x) $ batch environment state