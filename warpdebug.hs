import Network.Wai.Handler.Warp (run)
import ReacocardNet

main = withReacocardNet $ run 3000
