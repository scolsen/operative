#!/usr/bin/env stack
-- stack --resolver lts-6.25 script

import qualified MinParse as MP

-- Use record syntax to define optons.

options :: [OptDescr Flag]
options = [
            help "test",
            version "1.0.0",
            option (params {charIdens = ['r'] stringIdens = ["reverse"] usage = "String to reverse" argType = "STRING"})
          ]

rvrs = map reverse

main = MP.parseArgs options
       >>= return (rvrs (MP.getArgByIden parsed "reverse"))
       >>= print
