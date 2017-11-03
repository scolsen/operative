module MinParse (
                  parsed,
                  parsedOpts,
                  nonOpts,
                  parse  
                ) where

import System.Console.GetOpt
import Data.Maybe

parsed :: ([a], [String], [String]) -> ([a], [String])
parsed trip = (parsedOpts trip, nonOpts trip)

parsedOpts :: ([a], [String], [String]) -> [a]
parsedOpts (a, _, _) = a

nonOpts :: ([a], [String], [String]) -> [String]
nonOpts (_, a, []) = a
nonOpts (_, _, errs) = errs

parse :: [OptDescr Flag] -> [String] -> ([Flag], [String], [String])
parse options args = getOpt RequireOrder options args

