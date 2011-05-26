import ReacocardNet
import Network.Wai.Handler.FastCGI (run)

main :: IO ()
main = withReacocardNet $ run
