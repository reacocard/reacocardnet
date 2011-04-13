
import ReacocardNet
import Yesod (toWaiApp)
import Network.Wai.Handler.FastCGI (run)
main = toWaiApp ReacocardNet >>= run
