module GitObj where
data GitObj = Milestone | Issue
instance Show GitObj where
  show Milestone = "milestones"
  show Issue = "issues"
type User = String
