{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Milestone where
import Data.Aeson
import qualified GHC.Generics as G
import GitClient
import GitObj
import Control.Lens
import Data.Aeson.Lens
import Control.Monad.Except
import Data.Scientific
import qualified Data.Vector as V
import qualified Data.Text as T
data NewMilestone = NewMilestone{
  title :: String,
  state :: String,
  due_on :: String,
  description :: String
  } deriving (G.Generic,ToJSON)
  
mkNewMilestone :: Value -> Either String Value
mkNewMilestone val = 
  maybe (Left $ "keys title,state,due_on or description not found in " ++ show val) (Right . toJSON) $
   NewMilestone
   <$> (val ^? key "title" . _String . to T.unpack)
   <*> (val ^? key "state" . _String . to T.unpack)
   <*> (val ^? key "due_on" . _String . to T.unpack)
   <*> (val ^? key "description" . _String . to T.unpack)

number :: Value -> Either String Int
number val = do
  maybe (Left $ "key 'number' not found in " ++ show val) (Right) $
    (val ^? key "number" . _Number >>= toBoundedInteger)

mkNewIssue :: Value -> Either String Value
mkNewIssue val = 
  maybe (Left $ "keys title,body,milestone,labels found in " ++ show val) (Right) $
  (\title body milestone labels ->
      object[("title",title),
             ("body",body),
              ("milestone",milestone),
              ("labels",labels)])
  <$> (val ^? key "title")
  <*> (val ^? key "body")
  <*> (val ^? key "milestone")
  <*> (val ^? key "labels")

mkPatchIssue :: Value -> [User] -> Either String Value
mkPatchIssue val users =   
  maybe (Left $ "keys state,assignees,assignee" ++ show val) (Right) $
  (\state assignees  ->
      object[("state" , state),
             ("assignees", assignees)])
  <$> (val ^? key "state")
  <*> do
   a <- (val ^? key "assignee")
   as <- return (val ^.. key "assignees" )
   return $ Array $ V.fromList $ filter (`notElem` (String . T.pack <$> users)) $ a : as
