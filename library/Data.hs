module Data.Operative where

type HelpString  = Maybe String
type LongOption  = String
type ShortOption = Maybe Char

data OptionType = Argument
                | Flag
                deriving (Eq, Show)

data Option = Option LongOption ShortOption OptionType HelpString
            deriving (Eq, Show)

data Runtime = Runtime (Maybe Option) (Maybe String)
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

quickOptions :: [(String, OptionType)] -> [Option]
quickOptions (x:xs) = (Option (fst x) Nothing (snd x) Nothing):quickOptions xs
quickOptions [] = []
