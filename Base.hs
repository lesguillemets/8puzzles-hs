module Base where
import Control.Arrow
import Data.Array.Unboxed as U
import Data.List

size = 3 :: Int

type Field = U.UArray Loc Int
type Loc = (Int,Int)

data Dir = Up | Rt | Dw | Lf deriving (Show, Enum, Eq)
toVect :: Dir -> (Int,Int)
toVect Up = (-1,0)
toVect Rt = (0,1)
toVect Dw = (1,0)
toVect Lf = (0,-1)

data Puzzle = Puzzle {
            _field :: Field,
            _zloc :: Loc
}

fromList :: [Int] -> Puzzle
fromList xs = let
    f = listArray ((0,0),(size-1,size-1)) xs
    in
        Puzzle {
               _field = f,
               _zloc = (`divMod` size) .  head  $ 0 `elemIndices` xs
               }


instance Show Puzzle where
    show = showTable . _field

showTable :: Field -> String
showTable = unlines . map (tail . concatMap ('|':)) . cutEvery size
    . map show . elems

cutEvery :: Int -> [a] -> [[a]]
cutEvery _ [] = []
cutEvery n xs = let (pre,post) = splitAt n xs
                    in pre : cutEvery n post

movableDirs :: Puzzle -> [Dir]
movableDirs p = let
    (y,x) = _zloc p
    in
        filter (
               case x of
                   0 -> (/= Lf)
                   2 -> (/= Rt)
                   _ -> const True
              )
        . filter (
                 case y of
                    0 -> (/= Up)
                    2 -> (/= Dw)
                    _ -> const True
                ) $ enumFrom Up

move :: Puzzle -> Dir -> Puzzle
move p d = let
    next = toVect d `tupAdd` _zloc p
    nexp = _field p // [(next,0), (_zloc p, _field p ! next)]
    in
        p {
          _field = nexp,
          _zloc = next
        }

tupAdd :: (Num a, Num b) => (a,b) -> (a,b) -> (a,b)
tupAdd (x,y) = (+x) *** (+y)

nextPossibles :: Puzzle -> [Puzzle]
nextPossibles p = map (move p) (movableDirs p)

isComplete :: Puzzle -> Bool
isComplete = (== [1,2,3,4,5,6,7,8,0]) . elems . _field

data Phase = Phase {_puz :: Puzzle, _actualCost :: Int}
instance Show Phase where
    show p = unlines ["Phase :", show (_puz p), "cost" ++ show (_actualCost p)]
updatePhase :: Phase -> Puzzle -> Phase
updatePhase ph pz = Phase {_puz = pz, _actualCost = _actualCost ph +1}
nextPhases :: Phase -> [Phase]
nextPhases ph = let
    p = _puz ph
    c = _actualCost ph in
        map (\pz -> Phase pz (c+1)) . nextPossibles $ p

-- main = do
--     let a = fromList [1,0,3,2,4,5,6,7,8]
--     print a
--     print $ _zloc a
--     print $ movableDirs a
--     print $ move a Dw
--     print $ elems . _field $ a
