data Identifiers = Identifiers Char String

data Option = Flag Identifiers Bool
            | Single Identifiers String
            | Variad Identifiers [String]
            deriving (Eq, Show)

data Program = Spec {
                help :: String,
                version :: String,
                usage :: String,
                options :: [Option] 
               } deriving Show


data Argument = Valid String [String]
              | Invalid String [String]

parse :: Program -> [String] -> [Argument]

valid? :: Argument -> Bool
valid? (Valid _ _) = True
valid? (Invalid _ _) = False

validArguments :: [Argument] -> [Argument]
validArguments xs = filter valid?
