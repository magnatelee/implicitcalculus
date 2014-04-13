module Main where

import CoreElaboration
import SourceElaboration
import SrcParser
import SyntaxSource
import SystemFSemantics
import Types
import System.Environment

parseAndEval filename = do
  putStrLn $ "Source file: " ++ filename
  pgmText <- readFile filename
  case srcparse pgmText of
       Nothing -> putStrLn "Error: syntax error"
       Just pgm -> do
         putStrLn "#######################################"
         putStrLn "#          Source Program             #"
         putStrLn "#######################################"
         putStrLn (showPgm pgm)
         let pgm' = translateSrcPgm pgm
         putStrLn "#######################################"
         putStrLn "#          Implicit Calculus          #"
         putStrLn "#######################################"
         putStrLn (show pgm')
         putStrLn ""
         case translate pgm' of
           Nothing -> putStrLn "Error: failed to translate into System F"
           Just pgm'' -> do
             putStrLn "#######################################"
             putStrLn "#              System F               #"
             putStrLn "#######################################"
             putStrLn (show pgm'')
             putStrLn ""
             case evalAll pgm'' of
               Nothing -> putStrLn "Error: failed to evaluate the resulting System F program"
               Just result -> putStrLn ("Evaluation result: " ++ show result)
             

main = do
  putStrLn "#######################################"
  putStrLn "#    Implicit Calculus Interpreter    #"
  putStrLn "#######################################"
  putStrLn ""
  args <- getArgs
  if length args < 1
    then putStrLn "Error: source file should be given"
    else parseAndEval (args !! 0)
