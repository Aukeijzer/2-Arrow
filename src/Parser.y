{
module Parser where

import Model
}

%name parser
%tokentype { Token }
%error { happyError }

%token
  "->"        { ArrowToken }
  '.'         { PointToken }
  ','         { CommaToken }
  go          { GoToken }
  take        { TakeToken }
  mark        { MarkToken }
  nothing     { NothingToken }
  turn        { TurnToken }
  case        { CaseToken }
  of          { OfToken }
  end         { EndToken }
  left        { LeftToken }
  right       { RightToken }
  front       { FrontToken }
  ';'         { SemicolonToken }
  empty       { EmptyToken }
  lambda      { LambdaToken }
  debris      { DebrisToken }
  asteroid    { AsteroidToken }
  boundary    { BoundaryToken }
  '_'         { UnderscoreToken }
  identifier  { IdentToken $$ } 

%%

Program : Rule         { [$1] }
        | Program Rule { $2 : $1 }

Rule : identifier "->" Commands '.' { ($1,$3) }

Commands : {- empty -}          { [] }
         | Command              { [$1] }
         | Commands ',' Command { $3 : $1 }

Command : go                          { Go }
        | take                        { Take }
        | mark                        { Mark }
        | nothing                     { NothingCommand }
        | turn Direction              { Turn $2 }
        | case Direction of Alts end  { CaseOf $2 $4 }
        | identifier                  { Identifier $1 }

Direction : left  { LeftDir }
          | right { RightDir }
          | front { FrontDir }

Alts : {- emtpy -} { [] }
     | Alt           { [$1] }
     | Alts ';' Alt { $3 : $1 }

Alt : Pat "->" Commands { ($1,$3) }

Pat : empty     { Has Empty }
    | lambda    { Has Lambda }
    | debris    { Has Debris }
    | asteroid  { Has Asteroid }
    | boundary  { Has Boundary }
    | '_'       { Any }

{

happyError _ = error "parse error"

}