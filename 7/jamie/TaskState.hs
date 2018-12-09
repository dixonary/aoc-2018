{-# LANGUAGE RecordWildCards, FlexibleContexts #-}
module TaskState where

import Control.Monad (liftM2)
import Control.Monad.State.Strict (State, get, modify)
import Data.List (nub, sort, (\\))
import Data.Maybe (catMaybes)
import Data.Bool (bool)
import qualified Data.Map as Map
import Data.Map (Map)

type Time           = Int
type Task           = Char
type Worker         = Maybe (Task, Time)
type DependencyList = Map Task [Task]

type TaskState a = State Run a

data Run = Run 
        { tasks          :: [Task]
        , finished       :: [Task]
        , dependencyList :: DependencyList
        , workers        :: [Worker]
        , time           :: Time
        } deriving (Show)

initState :: DependencyList -> Int -> Run
initState list nw = Run
     { tasks          = sort . nub $ Map.keys list ++ (concat $ Map.elems list)
     , finished       = []
     , dependencyList = list
     , workers        = replicate nw Nothing
     , time           = 0
     }

-- Useful functions
finishTask :: Task -> TaskState ()
finishTask t = do
    Run{..} <- get
    setFinishedTasks  $ t : finished
    setDependencyList $ Map.delete t dependencyList 

completableTasks :: TaskState [Task]
completableTasks = do
    Run{..}  <- get
    let dependent = nub $ concat $ Map.elems $ dependencyList
    return $ tasks \\ (finished ++ dependent)

assignedTasks :: TaskState [Task]
assignedTasks = map fst <$> catMaybes <$> workers <$> get

readyTasks :: TaskState [Task]
readyTasks = liftM2 (\\) completableTasks assignedTasks


-- These could be lenses but fuck that
setDependencyList l = modify $ \st -> st { dependencyList = l }
setFinishedTasks ts = modify $ \st -> st { finished = ts }
setWorkers ws       = modify $ \st -> st { workers = ws }
setTime t           = modify $ \st -> st { time = t }
