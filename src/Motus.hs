module Motus where

import Data.Set (Set)

data Status = NotUsed | Misplaced | Correct
  deriving (Show, Eq, Enum, Bounded, Ord)

data Solution = Solution
  { _student :: String,
    _check :: String -> String -> [Status],
    _solve :: Set String -> [(String, [Status])] -> String
  }
