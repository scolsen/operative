module MinParse (
                  Identifiers(..),
                  Flag(..),
                  Parsed,
                  parse,
                  getParsed,
                  parsedIO,
                  parseArgs,
                  parsedFlags,
                  parsedNonOpts,
                  parsedValues,
                  getOptByIden,
                  getOptsByIden,
                  getArgByIden,
                  getAllIdentifiers,
                  getArg,
                  getFlg,
                  getIdentifiers,
                  hasIdentifier,
                  optArg,
                  reqArg,
                  noArg,
                  putVersion,
                  putHelp,
                  displayHelp,
                  displayVersion
                ) where

import System.IO
import System.Environment
import System.Exit
import System.Console.GetOpt
import Control.Monad
import Data.Maybe
import Data.List
import Data.Maybe

data Identifiers = Identifiers [Char] [String]
                    deriving(Eq, Show)
unIdentifiers :: Identifiers -> ([Char], [String])
unIdentifiers (Identifiers a b) = (a, b)

unIdenChar :: Identifiers -> [Char]
unIdenChar (Identifiers a _) = a

unIdenStr :: Identifiers -> [String]
unIdenStr (Identifiers _ a) = a

data Flag = Verbose 
          | Help String
          | Version String
          | Flg Identifiers Bool
          | Opt Identifiers String 
          deriving (Eq, Show)

type GetOptTuple = ([Flag], [String], [String])
type Parsed      = ([Flag], [String])

-- Core. "Returns" Parsed
parse :: [OptDescr Flag] -> [String] -> ([Flag], [String], [String])
parse options args = getOpt RequireOrder options args

getParsed :: GetOptTuple -> Parsed
getParsed (a, b, [])   = (a, b)
getParsed (a, _, errs) = (a, errs)

parsedIO :: GetOptTuple -> IO Parsed
parsedIO tup = return (getParsed tup)

parseArgs :: [OptDescr Flag] -> IO Parsed -- getArgs then parse 
parseArgs opts = getArgs >>= parsedIO . parse opts

-- Parsed
parsedFlags :: Parsed -> [Flag]
parsedFlags (a, _) = a

parsedNonOpts :: Parsed -> [String]
parsedNonOpts (_, b) = b

parsedValues :: Parsed -> ([String], [String])
parsedValues (a, b) = (catMaybes (map getArg a), b) 

getOptByIden :: Parsed -> String -> [Flag]
getOptByIden x str = filter (\y -> hasIdentifier y (Nothing, Just str)) (parsedFlags x)

getOptsByIden :: Parsed -> [String] -> [Flag]
getOptsByIden p strs = join (map (\y -> (getOptByIden p y)) strs)

getArgByIden :: Parsed -> String -> [Maybe String]
getArgByIden x str = map getArg (getOptByIden x str)

getAllIdentifiers :: Parsed -> [Identifiers]
getAllIdentifiers x = map getIdentifiers (parsedFlags x)

-- Flag
getArg :: Flag -> Maybe String
getArg (Opt i a) = Just a
getArg _         = Nothing

getFlg :: Flag -> Maybe Bool
getFlg (Flg i b) = Just b
getFlg _         = Nothing

getIdentifiers :: Flag -> Identifiers
getIdentifiers (Opt i _) = i
getIdentifiers (Flg i _) = i
getIdentifiers _         = Identifiers [] []

hasIdentifier :: Flag -> (Maybe Char, Maybe String) -> Bool
hasIdentifier (Opt i _) (Just a, Just b) = or ((any (a ==) (unIdenChar i)):(any (b ==) (unIdenStr i)):[])
hasIdentifier (Opt i _) (Nothing, Just b)        = or (False:(any (b ==) (unIdenStr i)):[])
hasIdentifier (Opt i _) (Just a, Nothing)        = or ((any (a ==) (unIdenChar i)):False:[])
hasIdentifier (Flg i _) (Just a, Just b) = or ((any (a ==) (unIdenChar i)):(any (b ==) (unIdenStr i)):[])
hasIdentifier (Flg i _) (Nothing, Just b)        = or (False:(any (b ==) (unIdenStr i)):[])
hasIdentifier (Flg i _) (Just a, Nothing)        = or ((any (a ==) (unIdenChar i)):False:[])

-- Identifiers
optArg :: [Char] -> [String] -> String -> String -> String -> OptDescr Flag
optArg c s def alt use = 
    Option c s (OptArg (Opt (Identifiers c s) . fromMaybe def) alt) use

reqArg :: [Char] -> [String] -> String -> String -> OptDescr Flag
reqArg c s alt use = 
    Option c s (ReqArg (Opt (Identifiers c s)) alt) use

noArg :: [Char] -> [String] -> Bool -> String -> OptDescr Flag
noArg c s def use = 
    Option c s (NoArg (Flg (Identifiers c s) def)) use

-- Display
putHelp :: Maybe Flag -> IO ()
putHelp (Just (Help a)) = putStrLn a >> exitWith(ExitFailure 1)
putHelp Nothing         = putStrLn "No help documentation provided."

putVersion :: Maybe Flag -> IO ()
putVersion (Just (Version a)) = putStrLn a >> exitWith(ExitFailure 1)
putVersion Nothing            = putStrLn "Unknown Version"

displayHelp :: Parsed -> String -> IO ()
displayHelp (a, _) x = putHelp (find (== Help x) a)

displayVersion :: Parsed -> String -> IO ()
displayVersion (a, _) x = putVersion (find (== Version x) a)

