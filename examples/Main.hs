-- Example use of MinParse
import qualified MinParse as MP

main :: IO ()
main = MP.parseArgs
     >>= print
