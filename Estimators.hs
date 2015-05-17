module Estimators where
import Base
import Data.Array.Unboxed as U
diffsNum :: Puzzle -> Int
diffsNum = length . filter (\(x,y)-> y /= 0  && x /= y)
    . zip [1,2,3,4,5,6,7,8,0] . elems . _field

manHattan :: Puzzle -> Int
manHattan = sum . map (\((x0,y0),e) -> let (x1,y1) = (e-1) `divMod` 3
                                   in
                                      abs (x1-x0) + abs (y1-y0))
                                      . filter ((/=0) . snd) . assocs . _field
