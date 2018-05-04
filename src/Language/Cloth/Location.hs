module Language.Cloth.Location(
  Location(..), Located(..), advanceLeft, advanceLine, item, intoLocated, (<@>), changeLocationTo,
  fstLocation, WithLocation(..)
) where

import Data.Default (Default(..))

data Location = Location { line :: Int, left :: Int } deriving Eq
instance Show Location where show p = "line " ++ show (line p) ++ ":column " ++ show (left p)
advanceLeft, advanceLine :: Location -> Location
advanceLeft p = p { left = left p + 1 }
advanceLine p = Location (line p + 1) 1
instance Default Location where def = Location 1 1
data Located a = a :@: Location deriving Eq
instance Show a => Show (Located a) where show (a :@: p) = show a ++ " at " ++ show p
instance Functor Located where fmap f (a :@: b) = f a :@: b
instance Applicative Located where
  pure v = v :@: def
  (f :@: p) <*> (v :@: _) = (f v) :@: p
item :: Located a -> a
item (v :@: _) = v
intoLocated :: a -> Located a
intoLocated a = a :@: def
(<@>) :: Located a -> Location -> Located a
(v :@: _) <@> p = v :@: p
changeLocationTo :: Location -> Located a -> Located a
changeLocationTo = flip (<@>)
fstLocation :: (Located a, Located b) -> Located (a, b)
fstLocation (a :@: p, b :@: _) = (a, b) :@: p

class WithLocation a where
  location :: a -> Location
instance WithLocation (Located a) where
  location (_ :@: p) = p
