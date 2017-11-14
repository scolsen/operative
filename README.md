# [minParse][]

MinParse is a small wrapper around the System.Console.getOpt module. It's goal is to reduce some of the boilerplate involved in a traditional use of get opt. Typically, one has to either define a data type to represent getOpt flags, or use record syntax to do so. MinParse provides a prebaked, but flexible data type Flag to bypass this.

## Types

MinParse exposes the following type definitions:

### Identifiers

Definition: `data Identifiers = Identifiers [Char] [String]`
Derivations: `deriving(Eq, Show)`

The Identifiers data type is used to maintain a refernce to the [Char] and [String] option identifiers passed to getOpt. It's necessary to maintain this information on the Flag data type in order to preserve an association between the a named option and the values it is passed at runtime. 

### Flag

Definition: ```haskell
            data Flag = Verbose
                      | Help String
                      | Version String
                      | Flg Identifiers Bool
                      | Opt Identifiers String
            ```
Derivations: `deriving(Eq, Show)`

Thanks for starting a project with Haskeleton! If you haven't heard of it
before, I suggest reading the introductory blog post. You can find it here:
<http://taylor.fausak.me/2014/03/04/haskeleton-a-haskell-project-skeleton/>.



``` sh
# Build the project.
stack build

# Run the test suite.
stack test

# Run the benchmarks.
stack bench

# Generate documentation.
stack haddock
```

Thanks again, and happy hacking!

[minParse]: https://github.com/scolsen/minParse 
