# copyRepoIssues
Usage: copyRepoIssues-exe --fromRepo USER/REPO --toRepo USER/REPO
                          --users USER1,USER2,.. --token oauthtoken
  Copy issues from one gitrepo to another keeping milestones and assignments of
  the issues for specified users. To not keep assignments, use --users "" - that
  way it will not assign issues in the destination repo. Transfer will fail if
  there are already milestone with the same name in the destination repo, or if
  specified users are not contributor to the destination repo.
