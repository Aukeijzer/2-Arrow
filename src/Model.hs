module Model where

-- Exercise 1
data Token = ArrowToken
           | PointToken
           | CommaToken
           | GoToken
           | TakeToken
           | MarkToken
           | NothingToken
           | TurnToken
           | CaseToken
           | OfToken
           | EndToken
           | LeftToken
           | RightToken
           | FrontToken
           | SemicolonToken
           | EmptyToken
           | LambdaToken
           | DebrisToken
           | AsteroidToken
           | BoundaryToken
           | UnderscoreToken
           | IdentToken String
           deriving Show


-- Exercise 2
type Program = [Rule]

type Rule = (String,[Command])

data Command = Go
             | Take
             | Mark
             | NothingCommand
             | Turn Direction
             | CaseOf Direction [Alternative]
             | Identifier String
             deriving (Show,Eq)

data Direction = LeftDir
               | RightDir
               | FrontDir
               deriving (Show,Eq)
type Alternative = (Pattern,[Command])

data Contents = Empty
    | Lambda
    | Debris
    | Asteroid
    | Boundary
    deriving (Show,Eq)

data Pattern = Has Contents
             | Any
             deriving (Show,Eq)
