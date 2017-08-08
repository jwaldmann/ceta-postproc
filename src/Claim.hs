module Claim where

import Text.Parsec hiding (upper,lower)
import Text.Parsec.String

parse s = Text.Parsec.parse claim "<>" s 

-- | specification: http://cbr.uibk.ac.at/competition/rules.php
data Claim = MAYBE 
  | WORST_CASE (Quest Lower) (Quest Upper)
  | YES
  | NO

data Quest a = None | Some a
data Lower = Omega Integer | NON_POLY
data Upper = O     Integer | POLY

instance Show Upper where 
  show POLY = "POLY"
  show (O n) = "O(n^" ++ show n ++ ")"

instance Show Lower where
  show NON_POLY = "NON_POLY"
  show (Omega n) = "Omega(n^" ++ show n ++ ")"

instance Show a => Show (Quest a) where
  show None = "?"
  show (Some a) = show a

instance Show Claim where
  show MAYBE = "MAYBE"
  show (WORST_CASE lo up) = "WORST_CASE(" ++ show lo ++ "," ++ show up ++ ")"
  show YES = "YES"
  show NO = "NO"


claim :: Parser Claim
claim = spaces *> (
      ( reserved "MAYBE" *> return MAYBE )
  <|> ( reserved "YES" *> return YES )
  <|> ( reserved "NO" *> return NO )
  <|> ( reserved "WORST_CASE" *> parens ( WORST_CASE <$> quest lower <*> (comma *> quest upper) ) )
    ) <* eof

quest p = ( reserved "?" >> return None ) <|> ( Some <$> p )

lower = ( reserved "NON_POLY" *> return NON_POLY )
  <|> ( reserved "Omega" *> ( Omega <$> parens degree ) )

upper = ( reserved "POLY" *> return POLY )
  <|> ( reserved "O" *> (O <$> parens degree ) )

degree =  ( reserved "1" *> return 0 )
  <|> ( reserved "n" >> reserved "^" *> natural ) 

reserved :: String -> Parser String
reserved s = string s <* spaces

comma :: Parser String
comma = reserved ","

parens :: Parser r -> Parser r
parens p = reserved "(" *> p <* reserved ")"

natural :: Parser Integer
natural = foldr1 (\ x y -> 10*x+y) 
  <$> many1 ( ( \ c -> fromIntegral $ fromEnum c - fromEnum '0') <$> digit )

