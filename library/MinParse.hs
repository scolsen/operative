module MinParse (
                  Identifiers(..),
                  Flag(..),
                  OptionGenerator(..),
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
                  option,
                  params,
                  help,
                  version,
                  putVersion,
                  putHelp,
                  displayHelp,
                  displayVersion,
                  info
                ) where

import System.IO 
import System.Environment 
import System.Exit 
import System.Console.GetOpt 
import Control.Monad 
import Data.Maybe 
import Data.List
import Data.Maybe

data OptionGenerator = Params {
                                   charIdens :: [Char],
                                   stringIdens :: [String],
                                   usage :: String,
                                   defaultArg :: String,
                                   argType :: String -- Printed using getOpt's usageInfo 
                              }
                              deriving Show
-- param updater/defaults
params = Params {charIdens = [], stringIdens = [], usage = undefined, defaultArg = "", argType = ""}

data Identifiers = Identifiers [Char] [String] deriving(Eq, Show)
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
hasIdentifier (Opt i _) (Just a, Just b)  = or ((any (a ==) (unIdenChar i)):(any (b ==) (unIdenStr i)):[])
hasIdentifier (Opt i _) (Nothing, Just b) = or (False:(any (b ==) (unIdenStr i)):[])
hasIdentifier (Opt i _) (Just a, Nothing) = or ((any (a ==) (unIdenChar i)):False:[])
hasIdentifier (Flg i _) (Just a, Just b)  = or ((any (a ==) (unIdenChar i)):(any (b ==) (unIdenStr i)):[])
hasIdentifier (Flg i _) (Nothing, Just b) = or (False:(any (b ==) (unIdenStr i)):[])
hasIdentifier (Flg i _) (Just a, Nothing) = or ((any (a ==) (unIdenChar i)):False:[])

-- Identifiers
unIdentifiers :: Identifiers -> ([Char], [String])
unIdentifiers (Identifiers a b) = (a, b)

unIdenChar :: Identifiers -> [Char]
unIdenChar (Identifiers a _) = a

unIdenStr :: Identifiers -> [String]
unIdenStr (Identifiers _ a) = a

-- OptionGenerator
optArg :: OptionGenerator -> OptDescr Flag
optArg args =
    _optArg (charIdens args) (stringIdens args) (defaultArg args) (argType args) (usage args)

reqArg :: OptionGenerator -> OptDescr Flag
reqArg args =
    _reqArg (charIdens args) (stringIdens args) (argType args) (usage args)

noArg :: OptionGenerator -> OptDescr Flag
noArg args =
    _noArg (charIdens args) (stringIdens args) True (usage args)

option :: OptionGenerator -> OptDescr Flag -- determine option to use based on record contents
option x 
    | (defaultArg x) /= "" = optArg x
    | (argType x) /= ""    = reqArg x
    | otherwise            = noArg x 


-- Display
help :: String -> OptDescr Flag
help x = Option ['h'] ["help"] (NoArg (Help x)) "Help"

version :: String -> OptDescr Flag
version x = Option ['V'] ["version"] (NoArg (Version x)) "Version"

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

-- synonyms 
info :: String -> [OptDescr Flag] -> String
info x y = usageInfo x y

-- Private
-- unwrap records to construct getOpt items
_optArg :: [Char] -> [String] -> String -> String -> String -> OptDescr Flag
_optArg c s def alt use = 
    Option c s (OptArg (Opt (Identifiers c s) . fromMaybe def) alt) use

_reqArg :: [Char] -> [String] -> String -> String -> OptDescr Flag
_reqArg c s alt use = 
    Option c s (ReqArg (Opt (Identifiers c s)) alt) use

_noArg :: [Char] -> [String] -> Bool -> String -> OptDescr Flag
_noArg c s def use = 
    Option c s (NoArg (Flg (Identifiers c s) def)) use

