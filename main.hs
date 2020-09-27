import Data.Char
import Data.Bifunctor

main = interact $
  unlines . map nameGame . lines
  -- unlines . map nameGame . const ["Mary", "Shirley", "Arnold", "Joe"]


nameGame :: String -> String
nameGame = unlines . apply songlines . repeat 
  where
    songlines = [line1, line2, line3, line4, line5]

makeLine fs = join " " . apply fs . repeat

line1 :: String -> String
line1 = makeLine [exclaim] 
line2 = makeLine [id, id, bo]
line3 = makeLine [const "bonana", const "fanna", fo]
line4 = makeLine [const "Fee fi", mo]
line5 = makeLine [exclaim] 


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


-- apply a list of functions to a list of inputs until either runs out
apply :: [a->b] -> [a] -> [b]
apply [] xs = []
apply fs [] = []
apply (f:fs) (x:xs) = (f x):apply fs xs

-- combine elements in a list together with a given separator
--  eg join " " ["Hello", "world"] = "Hello world"
join :: Monoid a => a -> [a] -> a
join _ [] = mempty
join _ [x] = x
join sep (x:xs) = x <> sep <> join sep xs

fork (f,g) x = (f x, g x)
