import Base
import Data.Array.Unboxed as U
import Data.Function
import Data.List

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


type SearchStack = [Phase]

aStarWithCount :: (Puzzle -> Int) -> Puzzle -> Maybe (Phase,Int)
aStarWithCount f p = aStarWithCount' f 0 [Phase p 0]

aStarWithCount' :: (Puzzle -> Int) -> Int -> SearchStack -> Maybe (Phase,Int)
aStarWithCount' _ _ [] = Nothing
aStarWithCount' f n ss = let
    -- next candidate is the phase where (actualcost + estimatedcost) is
    -- minimum
    ss' = sortBy (compare `on` (\p -> _actualCost p + f (_puz p))) ss
    best = head ss'
    nextPuz = nextPossibles $ _puz best
    in
        case find isComplete nextPuz of
            Just p
                -> Just (updatePhase best p, n+1)
            Nothing
                -> aStarWithCount' f (n+1) (
                        map (updatePhase best) nextPuz ++ tail ss')

aStarWithCountIO :: (Puzzle -> Int) -> Puzzle -> IO ()
aStarWithCountIO f p = aStarWithCountIO' f 0 [Phase p 0]
aStarWithCountIO' :: (Puzzle -> Int) -> Int -> SearchStack -> IO()
aStarWithCountIO' _ _ [] = putStrLn "FAIL"
aStarWithCountIO' f n ss = let
    -- next candidate is the phase where (actualcost + estimatedcost) is
    -- minimum
    ss' = sortBy (compare `on` (\p -> _actualCost p + f (_puz p))) ss
    best = head ss'
    nextPuz = nextPossibles $ _puz best
    in
        do
            mapM_ (\puz -> do
                        print $ _puz puz
                        print $ _actualCost puz + f (_puz puz)) ss'
            print $ map _actualCost ss'
            putStrLn " best is "
            print best
            print "__________________________"
            case find isComplete nextPuz of
                Just p
                    -> do
                        print (updatePhase best p, n+1)
                        putStrLn "SUCCESS"
                Nothing
                    -> aStarWithCountIO' f (n+1) (
                            map (updatePhase best) nextPuz ++ tail ss')

diffsNum :: Puzzle -> Int
diffsNum = length . filter (\(x,y)-> y /= 0  && x /= y)
    . zip [1,2,3,4,5,6,7,8,0] . elems . _field

manHattan :: Puzzle -> Int
manHattan = sum . map (\((x0,y0),e) -> let (x1,y1) = (e-1) `divMod` 3
                                   in
                                      abs (x1-x0) + abs (y1-y0))
                                      . filter ((/=0) . snd) . assocs . _field

main = do
    aStarWithCountIO diffsNum $ fromList [0,2,3,1,7,6,5,4,8]
    aStarWithCountIO manHattan $ fromList [0,2,3,1,7,6,5,4,8]
