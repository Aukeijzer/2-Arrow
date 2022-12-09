{
module Lexer where

import Model
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  
  $white+               ;
  [\r\n]+               ;
  "--".*                ;
  \-\>                  { const ArrowToken}
  \.                    { const PointToken }
  \,                    { const CommaToken }
  go                    { const GoToken }
  take                  { const TakeToken }
  mark                  { const MarkToken }
  nothing               { const NothingToken }
  turn                  { const TurnToken }
  case                  { const CaseToken }
  of                    { const OfToken }
  end                   { const EndToken }
  left                  { const LeftToken }
  right                 { const RightToken }
  front                 { const FrontToken }
  \;                    { const SemicolonToken }
  Empty                 { const EmptyToken }
  Lambda                { const LambdaToken }
  Debris                { const DebrisToken }
  Asteroid              { const AsteroidToken }
  Boundary              { const BoundaryToken }
  \_                    { const UnderscoreToken }
  [$alpha$digit\+\-]+ { \s -> IdentToken s }

