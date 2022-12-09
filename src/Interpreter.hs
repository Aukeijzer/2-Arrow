module Interpreter where

import ParseLib.Abstract
import Prelude hiding ((<*), (<$))
import Data.Map (Map)
import qualified Data.Map as L
import qualified Data.Set as Set
import qualified GHC.Utils.Misc as GHC
import Data.Char (isSpace)
import Control.Monad (replicateM)
import Data.List

import Lexer
import Parser
import Model
import Algebra

type Size      =  Int
type Pos       =  (Int, Int)
type Space     =  Map Pos Contents

-- | Parses a space file, such as the ones in the examples folder.
parseSpace :: Parser Char Space
parseSpace = do
    (mr, mc) <- parenthesised ((,) <$> natural <* symbol ',' <*> natural)
                <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <- replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ L.fromList $ concat $
            zipWith (\r cs ->
              zipWith (\c d -> ((r, c), d)) [0..] cs) [0..] css
  where
    spaces :: Parser Char String
    spaces = greedy (satisfy isSpace)

    contents :: Parser Char Contents
    contents = choice (Prelude.map (\(f,c) -> f <$ symbol c) contentsTable)
      <* spaces


-- | Conversion table
contentsTable :: [ (Contents, Char)]
contentsTable =  [ (Empty   , '.' )
                 , (Lambda  , '\\')
                 , (Debris  , '%' )
                 , (Asteroid, 'O' )
                 , (Boundary, '#' )]


-- Exercise 7
printSpace :: Space -> String
printSpace space = L.foldrWithKey f [] space
  where space2Char contents = foldr
          (\x xs -> if fst x == contents then snd x:xs else xs ) [] contentsTable
        ((xMax,yMax),_) = L.findMax space
        
        f :: Pos -> Contents -> String -> String
        f (_,xPos) contents s = space2Char contents ++ (if xPos == xMax then "\n" else []) ++ s

-- These three should be defined by you
type Ident = String
type Commands = [Command]
data Heading = North | East | South | West
  deriving (Show,Eq)

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment s = if checkProgram reversedProgram then L.fromList program else error "incorrect syntax for program, check your headers"
  where reversedProgram = parser $ alexScanTokens s
        --happy reverses your lists
        program = fold algebra reversedProgram
        reverseSecond (x,ys) = (x,reverse ys)
        algebra :: Algebra Program Command Direction Pattern
        algebra = ((map reverseSecond),
                  (Go,Take,Mark,NothingCommand,Turn, 
                  (\dir alts -> CaseOf dir (reverse $ map reverseSecond alts)) ,Identifier),
                  dirId,
                  patId)

-- | Exercise 9
step :: Environment -> ArrowState -> Step
step _ (ArrowState space pos heading []) = Done space pos heading
step enviroment state@(ArrowState space pos heading (command:stack)) = case command of
  Go -> Ok (ArrowState space newPos heading stack)
    where front = frontPos heading pos
          newPos = case atPos space front of
            Asteroid -> pos
            Boundary -> pos
            _ -> front
  
  Take -> Ok (ArrowState newSpace pos heading stack)
    where newSpace = L.update (const $ Just Empty) pos space
  
  Mark -> Ok (ArrowState newSpace pos heading stack)
    where newSpace = L.update (const $ Just Lambda) pos space
  
  NothingCommand -> Ok (ArrowState space pos heading stack)
  
  Turn dir -> Ok (ArrowState space pos newHeading stack)
    where newHeading = lookDir dir heading
  
  CaseOf dir alts -> case alt of
    Nothing -> Fail "Error while patternmatching"
    Just (predicate,commands) -> Ok (ArrowState space pos heading (commands ++stack))
    where contents = atPos space (frontPos (lookDir dir heading) pos)
          alt = matchAlts contents alts
  
  Identifier ident -> case maybeCommand of
      Nothing -> Fail "Call to unknown command"
      Just commands -> Ok (ArrowState space pos heading (commands++stack))
    where maybeCommand = L.lookup ident enviroment

--Helper functions used in step
atPos :: Space -> Pos -> Contents
atPos space pos@(xPos,yPos) = let ((xMax,yMax),_) = L.findMax space in
  if xPos > xMax || yPos > yMax then Boundary else
    case L.lookup pos space of
      Nothing -> Empty
      Just x -> x

frontPos :: Heading -> Pos -> Pos
frontPos heading (x,y) = case heading of
  North -> (x,y+1)
  East -> (x+1,y)
  South -> (x,y-1)
  West -> (x-1,y)

lookDir :: Direction -> Heading -> Heading
lookDir dir = case dir of
  LeftDir -> counterClockWise
  FrontDir -> id
  RightDir -> clockWise

clockWise :: Heading -> Heading
clockWise heading = case heading of
  North -> East
  East -> South
  South -> West
  West -> North

counterClockWise :: Heading -> Heading
counterClockWise heading = case heading of
  North -> West
  West -> South
  South -> East
  East -> North

matchAlts :: Contents -> [Alternative] -> Maybe Alternative
matchAlts contents = find (f . fst)
  where f (Has x) = contents == x
        f Any = True