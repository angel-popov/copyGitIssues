module Lib
    ( copyRepos
    ) where
import Config
import Data.List.Split
import Data.Aeson
import Milestone
import Repo
import GitClient
import GitObj
import Control.Monad.Except
import Control.Monad.Reader

type MilestoneMap = [(Int,Int)]
copyFrom :: Int -> (Int -> Resp r) -> Resp [r]
copyFrom n fn = do
  conf <- ask
  res <- liftIO $ runExceptT $ runReaderT (fn n) conf
  case res of
    (Left err) -> return []
    (Right res) -> do
      copyFrom (n+1) fn >>= return . (res:)

copyMilestone :: Int -> Resp (Int,Int)
copyMilestone no = do
  from <- FromRepo <$> asks from
  to <- ToRepo <$> asks to 
  newNum <- (getJSON Milestone from no)
    >>= liftEither . mkNewMilestone
    >>= (putJSON Milestone to)
    >>= liftEither . number
  return (no, newNum)

copyIssue :: (FromJSON r)=>[User]->MilestoneMap -> Int -> Resp r
copyIssue users mm no = do
  from <- FromRepo <$> asks from
  to <- ToRepo <$> asks to 
  old <- getJSON Issue from no 
  num <- ((putJSON Issue to) . mkNewIssue $ old) >>= liftEither . number
  (liftEither $ mkPatchIssue old users) >>= patchJSON Issue to num

copyMilestones :: Resp MilestoneMap
copyMilestones = copyFrom 1 copyMilestone

copyIssues :: (FromJSON r)=> [User] -> MilestoneMap -> Resp [r]
copyIssues users mmap = copyFrom 1 (copyIssue users mmap)

copyAll :: (FromJSON r)=> [User] -> Resp [r]
copyAll users = do
  copyMilestones >>= copyIssues users

copyRepos :: Config -> IO ()
copyRepos c = do
  (runExceptT $ runReaderT (copyAll (splitOn "," $ users c) ) c) >>=
    either (putStrLn . ("ERROR:"++)) putStrLn
