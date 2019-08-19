module Main where

import Lib
import Repo
import Config
import Options.Applicative
import Data.Semigroup ((<>))

config :: Parser Config
config = Config
  <$> strOption (
          long "fromRepo"
          <> metavar "USER/REPO"
          <> help "Source repo for milestones and issues.")
  <*> strOption (
          long "toRepo"
          <> metavar "USER/REPO"
          <> help "Destination repo where issues will be copied.")
  <*> strOption (
          long "users"
          <> metavar "USER1,USER2,.."
          <> help "List of users that are already in both repos. They are used for assignments.")
  <*> strOption (
          long "token"
          <> metavar "oauthtoken"
          <> help "oAuth token used in gitapi. - see https://developer.github.com/apps/building-integrations/setting-up-and-registering-oauth-apps/")
  
main :: IO ()
main = copyRepos =<< execParser opts
  where
    opts = info (config <**> helper)
      ( fullDesc
        <> progDesc "Copy issues from one gitrepo to another keeping milestones and assignments of the issues for specified users. To not keep assignments, use --users \"\" - that way it will not assign issues in the destination repo.\n Transfer will fail if there are already milestone with the same name in the destination repo, or if specified users are not contributor to the destination repo."
        <> header "copyGitIssues - move issues from repo to repo" )

