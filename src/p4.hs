
import Data.List

nums = [(a,b,c,d,e,f) | a <- [0..9],
                        b <- [0..9],
                        c <- [0..9],
                        d <- [0..9],
                        e <- [0..9],
                        f <- [0..9],
                        b >= a,
                        c >= b,
                        d >= c,
                        e >= d,
                        f >= e]

adj = filter pre nums
  where
    pre (a,b,c,d,e,f) =
      let diff = [a - b, b - c, c - d, d - e, e - f]
      in (0 `elem` diff) && ([0] `elem` (groupBy (==) diff))

bottom = 234208
top = 765869

convert = map (\(a,b,c,d,e,f) -> 100000*a+10000*b+1000*c+100*d+10*e+f)
range = takeWhile (<top) . dropWhile (<bottom) . convert






