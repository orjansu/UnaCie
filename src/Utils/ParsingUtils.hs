
module ParsingUtils
 ( expectedError -- An errror message that specifies what was expected.
 ) where

{-
  Information:
  ---------------------------------1--------------------------------------------
  - Helper functions for parsing commands/contexts etc.
-}

-- Generate an error message specifying what was expected.
expectedError :: String -> String -> String
expectedError par ex = "param: '" ++ par ++ "', expected: " ++ ex
