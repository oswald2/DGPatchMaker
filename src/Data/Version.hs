{-# LANGUAGE
  OverloadedStrings
  , TemplateHaskell
#-}
module Data.Version
(versionString)
where

import Data.Text as T
import Development.GitRev



versionString :: Text
versionString = T.concat ["Version: 1.0 ", "Branch: ", $(gitBranch), " ", $(gitHash), "\ndirty: ", dirty, "\nCommit Date: ", $(gitCommitDate) ]

dirty :: Text
dirty | $(gitDirty) = "true"
      | otherwise = "false"
