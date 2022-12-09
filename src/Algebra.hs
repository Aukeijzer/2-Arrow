module Algebra where

import Model
import Data.Set (Set)
import qualified Data.Set as Set
import qualified GHC.Utils.Misc as GHC

-- Exercise 5
{- 
Explanation of algebra's:
The ProgramAlgebra is applied after the commandAlgebra is applied.
The CommandAlgebra is applied after the DirAlgebra and/or PatternAlgebra is applied ( if applicable (: )
What this means is that we use a fold over all Directions and Patterns, which gives two result types dirR and patR.
As directions and patterns only consist of constants, this is more akin to pattern matching values.
Commands can consist of directions and patterns, however since we also fold over those datatypes the CommandAlgebra uses the results of those types instead.
We fold the commands and then the ProgramAlgebra will use the results of those folded commands and turn them into the final result.
The fact why the algebra uses 4 type parameters instead of just one (the result) is because of the in-between types given by the other folds.

If one doesn't want to fold over patterns and/or directions the identity fold (commandId,patId) can be used.
-}

type ProgramAlgebra a commandR = [(String,[commandR])] -> a

type CommandAlgebra a dirR patR = (
    a,a,a,a,
    dirR -> a,
    dirR -> [(patR,[a])] -> a,
    String -> a)

type Algebra result commandR dirR patR = 
    (ProgramAlgebra result commandR,
    CommandAlgebra commandR dirR patR,
    DirAlgebra dirR,
    PatternAlgebra patR)

fold :: Algebra result commandR dirR patR -> Program -> result
fold (programAlg,commandAlg,dirAlg,patAlg) = fold
  where fold program = programAlg $ GHC.mapSnd (map foldCommand) program
        foldCommand c = 
          let (go,take,mark,nothing,turn,caseOf,ident) = commandAlg in
          case c of
            Go -> go
            Take -> take
            Mark -> mark
            NothingCommand -> nothing
            Turn dir -> turn (foldDir' dir)
            CaseOf dir alts -> caseOf (foldDir' dir)
              (GHC.mapFst foldPat' (GHC.mapSnd (map foldCommand) alts))
            Identifier s -> ident s
        foldDir' = foldDir dirAlg
        foldPat' = foldPattern patAlg

commandId :: CommandAlgebra Command Direction Pattern
commandId = (Go,Take,Mark,NothingCommand,Turn,CaseOf,Identifier)

--Other folds (used in main fold)
type DirAlgebra a = (a,a,a)
foldDir :: DirAlgebra a -> Direction -> a
foldDir (left,right,front) dir = 
    case dir of
      LeftDir -> left
      RightDir -> right
      FrontDir -> front

dirId :: DirAlgebra Direction
dirId = (LeftDir,RightDir,FrontDir)

type PatternAlgebra a = (a,a,a,a,a,a)
foldPattern :: PatternAlgebra a -> Pattern -> a
foldPattern (empty,lambda,debris,asteroid,boundary,any) p =
    case p of
        Has Empty -> empty
        Has Lambda -> lambda
        Has Debris -> debris
        Has Asteroid -> asteroid
        Has Boundary -> boundary
        Any -> any
patId :: PatternAlgebra Pattern
patId = (Has Empty,Has Lambda,Has Debris,Has Asteroid,Has Boundary,Any)

-- Exercise 6
allRules :: Program -> Set String
allRules = Set.fromList . (map fst)

allCalls :: Program -> Set String
allCalls = Set.fromList . (fold callAlgebra)
    where callAlgebra :: Algebra [String] [String] Direction Pattern
          callAlgebra = 
            ((concat . concat . (map snd)),
            ([],[],[],[],const [], \_ x -> concat $ concat $ map (snd) x, \x -> [x]),
            dirId,
            patId)

checkProgram :: Program -> Bool
checkProgram program = checkStart && checkDuplicates && checkCalls
  where checkStart = elem "start" rules
        --If the set has the same length then there must be no duplicates
        checkDuplicates = length program == (length rules)
        --If the calls are a subset of the rules a call to a undefined rule can't be made.
        checkCalls = Set.isSubsetOf calls rules
        rules = allRules program
        calls = allCalls program
