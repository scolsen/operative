-- Example use of Operative
import qualified Operative as O

main :: IO ()
main = O.parseArgs
       >>= print
