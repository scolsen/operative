module Operative where

import qualified Data.List as L
import System.Environment

import Data.Maybe 

-- Data
type HelpString  = Maybe String
type LongOption  = String
type ShortOption = Maybe Char

data OptionType = Argument
                | Flag
                deriving (Eq, Show)


data OptionSpec = OptionSpec LongOption ShortOption OptionType HelpString
                deriving (Eq, Show)

data Option = Arg String
            | Flg
            deriving (Eq, Show)

data RuntimeOption = RuntimeOption String Option
             deriving (Eq, Show)

optionType :: Option -> OptionType
optionType (Option _ _ o) = o

optionLong :: Option -> LongOption
optionLong (Option s _ _) = s

optionShort :: Option -> ShortOption
optionShort (Option _ (Just c) _) = c
optionShort (Option _ Nothing _)  = Nothing

optionHelp :: Option -> HelpString
optionHelp (Option _ _ _ (Just h)) = h
optionHelp (Option _ _ _ Nothing)  = Nothing

-- Functions

isOption :: String -> Option -> Bool
isOption x (Option s y _) = fromMaybe False $ (||) <$> Just (x == s) 
                                                   <*> ((==head x) <$> y)
getOption :: String -> [Option] -> Maybe Option
getOption s os = L.find (isOption s) os

getOptions :: [String] -> [Option] -> [RuntimeOption]
getOptions (x:y:xs) os 
           | any (isOption x) os = case optionType <$> opt of
                                 Just Argument -> (:) (Runtime Arg y) $ getOptions xs os
                                 Just Flag     -> (:) (Runtime Flg) $ getOptions (y:xs) os
           | otherwise  = getOptions (y:xs) os
           where opt = getOption x os
getOptions (x:[]) os = (Runtime (getOption x os) Nothing):getOptions [] os
getOptions []     _ = []

quickOptions :: [(String, OptionType)] -> [Option]
quickOptions (x:xs) = (Option Just (fst x) Nothing (snd x) Nothing):quickOptions xs
quickOptions [] = []

optionOf :: RuntimeOption -> Maybe Option
optionOf (RuntimeOption (Just x) _) = Just x
optionOf (RuntimeOption Nothing _) = Nothing

runtime :: [RuntimeOption] -> [RuntimeOption]
runtime xs = [x | x <- xs, \x -> isJust optionOf x]
