module Interpreter where

import ParseLib.Abstract
import Prelude hiding ((<*), (<$))

import Data.Map (Map)
import qualified Data.Map as L

import Data.Char (isSpace)
import Control.Monad (replicateM)

import Lexer
import Parser
import Model
import Algebra


data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary
  deriving (Show,Eq)

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
printSpace space = fst $ L.foldrWithKey f ([],(0,0)) space
  where space2Char contents = foldr
          (\x xs -> if fst x == contents then snd x:xs else xs ) [] contentsTable
        ((xMax,yMax),_) = L.findMax space
        
        f :: Pos -> Contents -> (String,Pos) -> (String,Pos)
        f newPos contents (s,oldPos) = (s ++ padding oldPos newPos ++ space2Char contents,newPos)
        padding (oldX,oldY) (newX,newY) = if oldY == newY 
          then replicate (newX - oldX) '.' 
          else replicate (xMax - oldX) '.' ++ "\n" ++ concat (replicate (newY - oldY - 1) emptyLine) ++ replicate newX '.'
            where emptyLine = replicate xMax '.' ++ "\n"


-- These three should be defined by you
type Ident = String
type Commands = [Command]
type Heading = ()

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment s = undefined
  where program = parser $ alexScanTokens s
         

-- | Exercise 9
step :: Environment -> ArrowState -> Step
step = undefined


