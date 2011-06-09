import Network.Wai.Handler.Warp (run)
import Controller

main = withReacocardNet $ run 3000
