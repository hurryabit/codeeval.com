{-# LANGUAGE FlexibleInstances #-}
import Control.Applicative hiding (empty)
import Control.Category
import Control.Monad
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP hiding (get)
import System.Environment

import Prelude hiding (id, (.))

data Person = Unknown | Known { name :: Int }
  deriving (Eq)

mkPerson :: Int -> Person
mkPerson 0 = Unknown
mkPerson n = Known n

isKnown :: Person -> Bool
isKnown Unknown   = False
isKnown (Known _) = True

data Direction = Enter | Leave
  deriving (Eq)

data Event a = Event {direction :: Direction, person :: Person, annotation :: a}
  deriving (Eq)

enter, leave :: Person -> a -> Event a
enter = Event Enter
leave = Event Leave

split :: Eq a => (a -> Bool) -> [a] -> Maybe ([a],a,[a])
split p xs = do
  let (ys,zs) = break p xs
  guard (not (null zs))
  return (ys,head zs,tail zs)

split3 :: Eq a => (a -> Bool) -> (a -> Bool) -> (a -> Bool) -> [a] -> Maybe ([a],a,[a],a,[a],a,[a])
split3 p q r xs = do
  (xs1,x1,ys1) <- split p xs
  (xs2,x2,ys2) <- split q ys1
  (xs3,x3,xs4) <- split r ys2
  return (xs1,x1,xs2,x2,xs3,x3,xs4)

replaceFirst, replaceLast :: Eq a => (a -> Bool) -> a -> [a] -> Maybe [a]
replaceFirst p b xs = do
  (ys,_,zs) <- split p xs
  return (ys ++ [b] ++ zs)
replaceLast x y = fmap reverse . replaceFirst x y . reverse        

matchBackdoor :: [Event Bool] -> Maybe [Event Bool]
matchBackdoor [] = return []
matchBackdoor (e1:es)
  | person e1 == Unknown || null gs || (direction e1 == Leave && direction (head gs) == Enter) = do
      es' <- matchBackdoor es
      return (e1:es')
  | direction e1 == Enter && direction (head gs) == Leave = do
      es' <- matchBackdoor (fs ++ (head gs) {annotation = True} : tail gs)
      return (e1 {annotation = True} : es')
  | otherwise = do
      let (e1',replace) = case direction e1 of
            Enter -> (e1 {annotation = True},replaceFirst (leave Unknown False ==) (leave psn True ))
            Leave -> (e1                    ,replaceLast  (enter Unknown False ==) (enter psn False))
      fs'  <- matchBackdoor fs
      fs'' <- replace fs'
      es'  <- matchBackdoor (fs'' ++ gs)
      return (e1':es')
  where
    (fs,gs) = break ((psn ==) . person) es
    psn = person e1

matchKnownEnters :: [Event Bool] -> [Event Bool]
matchKnownEnters [] = []
matchKnownEnters (e1:es)
  | direction e1 == Leave || person e1 == Unknown || annotation e1 == True || isNothing mes' =
      e1 : matchKnownEnters es
  | otherwise =
      e1 {annotation = True} : matchKnownEnters (fromJust mes')
  where mes' = replaceFirst (leave Unknown False ==) (leave (person e1) True) es

dual :: [Event a] -> [Event a]
dual = map (\e -> e {direction = mirror (direction e)}) . reverse
  where mirror Enter = Leave
        mirror Leave = Enter

matchKnownLeaves :: [Event Bool] -> [Event Bool]
matchKnownLeaves = dual . matchKnownEnters . dual

matchUnknown :: [Event Bool] -> [Event Bool]
matchUnknown [] = []
matchUnknown (e1:es)
  | direction e1 == Leave || person e1 /= Unknown || annotation e1 == True =
      e1 : matchUnknown es
  | otherwise = case replaceFirst (leave Unknown False ==) (leave Unknown True) es of
      Nothing  -> e1 : es
      Just es' -> e1 {annotation = True} : matchUnknown es'

rematch :: [Event Bool] -> [Event Bool]
rematch [] = []
rematch (e1:es)
  | direction e1 == Leave || annotation e1 == False || isNothing msplit = e1 : rematch es
  | otherwise = e1 : rematch (fs ++ [e2] ++ gs ++ [e1] ++ hs ++ [e2] ++ ks)
  where msplit@(~(Just (fs,lve,gs,ent,hs,e2,ks))) =
          split3 (leave Unknown False ==) (enter Unknown False ==) (leave (person e1) True ==) es

main = do
  arg1:_ <- getArgs
  input <- readFile arg1
  let processLine =
        maybe "CRIME TIME"
          ( show
          . length
          . filter (\e -> direction e == Enter && annotation e == False)
          . rematch
          . matchUnknown
          . matchKnownLeaves
          . matchKnownEnters
          )
          . matchBackdoor
          . map (\e -> e {annotation = False})
          . parse
  mapM_ (putStrLn . processLine) (lines input)

data Lens f a = Lens { get :: f -> a, modify :: (a -> a) -> f -> f }

lens = Lens

gets lns fun = fun . get lns

instance Category Lens where
  id = Lens { get = id, modify = id }
  lns1 . lns2 = lens (get lns1 . get lns2) (modify lns2 . modify lns1)

first = lens fst (\upd (x,y) -> (upd x,y))
second = lens snd (\upd (x,y) -> (x,upd y))


parse :: String -> [Event ()]
parse = fst . head . readP_to_S parseEvents
  where
    parseEvents = parseInt *> string "; " *> sepBy1 parseEvent (char '|') <* eof
    parseEvent = ((char 'E' *> pure enter) +++ (char 'L' *> pure leave)) <*>
      (char ' ' *> parsePerson) <*> pure ()
    parsePerson = mkPerson <$> parseInt
    parseInt = readS_to_P reads

instance Show Person where
  show Unknown   = "0"
  show (Known n) = show n

instance Show Direction where
  show Enter = "E"
  show Leave = "L"

instance Show (Event Bool) where
  show (Event dir psn ann) = concat
    [if ann then "*" else "", show dir, " ", show psn]
  showList es text = concat
    [show (length es), "; ", intercalate "|" (map show es), text]
