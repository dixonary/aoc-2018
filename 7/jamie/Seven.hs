{-# LANGUAGE RecordWildCards #-}
import Control.Monad (ap, liftM, join)
import Control.Monad.State.Strict (evalState, get)
import Data.Functor ((<&>))
import Data.Char (isUpper, ord)
import Data.List (foldl', sort)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import TaskState
import Debug.Trace

performTasksAlone :: TaskState [Task]
performTasksAlone = do
    Run{..} <- get
    todo    <- completableTasks
    case todo of
        (t:_) -> finishTask t >> performTasksAlone
        []    -> return $ reverse finished


assignTasks :: TaskState ()
assignTasks = do
    Run{..} <- get
    todo <- readyTasks
    
    let
        assignWorker ([],ws) w          = ([], w:ws)
        assignWorker (ts,ws) w@(Just _) = (ts, w:ws)
        assignWorker (t:ts,ws) Nothing  = (ts, Just (t, (time + duration t)):ws)

    setWorkers $ snd $ foldl' assignWorker (todo, []) workers
    return ()
          

finishAssignedTask :: TaskState ()
finishAssignedTask = do
    Run{..} <- get

    let 
        finishWorker Nothing = return Nothing
        finishWorker w@(Just (task, t)) = do
            Run{..} <- get
            if time == t 
                then finishTask task >> return Nothing
                else return w 

    setWorkers =<< (sequence $ map finishWorker workers)
    setTime $ minimum $ map snd $ catMaybes workers
    return ()


minDuration = 60
duration :: Task -> Time
duration t = ord t - ord 'A' + 1 + minDuration


performTasksWithWorkers :: TaskState Time
performTasksWithWorkers = do
    Run{..} <- get
    if (sort finished == tasks)
        then return time
        else assignTasks >> finishAssignedTask >> performTasksWithWorkers


main = do
    depList <-  readFile "input"
            <&> lines
            <&> map (drop 1)
            <&> map (filter isUpper)
            <&> map (\[a,b] -> (a,[b]))
            <&> M.fromListWith (++)

    let initialState = initState depList 5

    print $ evalState performTasksAlone       initialState
    print $ evalState performTasksWithWorkers initialState
