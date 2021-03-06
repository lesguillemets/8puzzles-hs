import Base
import Estimators
import Data.Array.Unboxed as U
import Data.Function
import Data.List

type SearchStack = [Phase]
type Estimator = Puzzle -> Int

aStarWithCount :: Estimator -> Puzzle -> Maybe (Phase,Int)
aStarWithCount f p = aStarWithCount' f 0 [Phase p 0]

aStarWithCount' :: Estimator -> Int -> SearchStack -> Maybe (Phase,Int)
aStarWithCount' _ _ [] = Nothing
aStarWithCount' f n ss = let
    -- next candidate is the phase where (actualcost + estimatedcost) is
    -- minimum
    ss' = sortBy (compare `on` cost f) ss
    best = head ss'
    nextPuz = nextPossibles $ _puz best
    in
        case find isComplete nextPuz of
            Just p
                -> Just (updatePhase best p, n+1)
            Nothing
                -> aStarWithCount' f (n+1) (
                        map (updatePhase best) nextPuz ++ tail ss')

aStarWithCountIO :: Estimator -> Puzzle -> IO ()
aStarWithCountIO f p = aStarWithCountIO' f 0 [Phase p 0]
aStarWithCountIO' :: Estimator -> Int -> SearchStack -> IO()
aStarWithCountIO' _ _ [] = putStrLn "FAIL"
aStarWithCountIO' f n ss = let
    -- next candidate is the phase where (actualcost + estimatedcost) is
    -- minimum
    ss' = sortBy (compare `on` cost f) ss
    best = head ss'
    nextPuz = nextPossibles $ _puz best
    in
        do
            mapM_ (\puz -> do
                        putStr . show $ _puz puz
                        print $ _actualCost puz + f (_puz puz)
                        putStrLn ""
                        ) ss'
            putStr " actual costs:"
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

idaStar :: Estimator -> Puzzle -> Maybe (Phase, Int)
idaStar f p = let
    iter :: Int -> Int -> [Phase] -> Maybe (Phase,Int)
    iter steps depth [] = iter steps (depth+1) [Phase p 0]
    iter steps depth ss =
        case find (isComplete . _puz) next of
            Just pu -> Just (pu,steps)
            Nothing -> iter (steps+1) depth next
        where
            next = filter ((<= depth) . cost f) . concatMap nextPhases $ ss
    in
        iter 0 1 [Phase p 0]

cost :: Estimator -> Phase -> Int
cost f p = _actualCost p + f (_puz p)

main = do
    let puz = fromList [0,2,3,1,7,6,5,4,8]
    aStarWithCountIO diffsNum puz
    aStarWithCountIO manHattan puz
    print . idaStar diffsNum $ puz
    print . idaStar manHattan $ puz
