# [minParse][]

MinParse is a small wrapper around the System.Console.getOpt module. It's goal is to reduce some of the boilerplate involved in a traditional use of get opt. Typically, one has to either define a data type to represent getOpt flags, or use record syntax to do so. MinParse provides a prebaked, but flexible data type Flag to bypass this.

## Data

MinParse exposes the following data definitions:

### Identifiers

```haskell
data Identifiers = Identifiers [Char] [String] deriving(Eq, Show)
```

The Identifiers data type is used to maintain a refernce to the [Char] and [String] option identifiers passed to getOpt. It's necessary to maintain this information on the Flag data type in order to preserve an association between the a named option and the values it is passed at runtime. 

### Flag

```haskell
    data Flag = Verbose
              | Help String
              | Version String
              | Flg Identifiers Bool
              | Opt Identifiers String
              deriving(Eq, Show)
```

### OptionGenerator

```haskell
data OptionGenerator = Params {
                                   charIdens :: [Char],
                                   stringIdens :: [String],
                                   usage :: String,
                                   defaultArg :: String,
                                   argType :: String  
                              }
                              deriving Show
```
The OptionGenerator is exposed for convenience, and can be used to construct an option using record syntax. While you can use OptionGenerator directly, it is more convient to use the option (params {..}) idiom to update a default OptionGenerator record and generate your option.

## Type Synonyms

### GetOptTuple

### Parsed

## Functions



[minParse]: https://github.com/scolsen/minParse 
