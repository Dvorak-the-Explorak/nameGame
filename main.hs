import Data.Char
import Data.Bifunctor
import Data.Function

main = interact $
  unlines . map nameGame . lines
  -- unlines . map nameGame . const ["Mary", "Shirley",  "Arnold", "Joe"]

nameGame :: String -> String
nameGame = unlines . apply songlines
  where
    songlines = [line1, line2, line3, line4, line5]

-- apply a list of functions to a single input, returning a list of outputs
apply fs = flip map fs . (&)

-- These take the name and produce the line of the song
line1 :: String -> String
line1 = makeLine [exclaim] 
line2 = makeLine [id, id, bo]
line3 = makeLine [const "bonana", const "fanna", fo]
line4 = makeLine [const "Fee fi", mo]
line5 = makeLine [exclaim] 

makeLine fs = join " " . apply fs

-- add an exclamation mark
exclaim = (++ "!")

bo = subName "B"
fo = subName "F"
mo = subName "M"

-- substitute the given start for the start of the name like: Shirley -> Bo-birley
--  if the starting letters are the same, leave out second sub like: Mary -> Mo-ary
subName :: String -> String -> String
subName sub name  | sub /= lead = sub ++ "o-" ++ (map toLower $ sub ++ rem)
                  | otherwise = sub ++ "o-" ++ (map toLower rem)
  where
    (lead, rem) = splitName name


-- splitName gives the bit that will be replaced in the song and the rest of the name
splitName = fork (takeWhile cons, dropWhile cons)
  where
    cons = not . flip elem vowels
vowels = "AEIOUaeiou"

-- combine elements in a list together with a given separator
--  eg join " " ["Hello", "world"] = "Hello world"
join :: Monoid a => a -> [a] -> a
join _ [] = mempty
join _ [x] = x
join sep (x:xs) = x <> sep <> join sep xs

fork (f,g) x = (f x, g x)
