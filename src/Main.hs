import Control.Monad
import Data.IORef
import Data.Time.Clock
import FRP.Yampa


twoSecondsPassed :: SF () Bool
twoSecondsPassed = time >>> arr (> 2)
 

main :: IO ()
main = do
  t <- getCurrentTime
  timeRef <- newIORef t
  let init        = putStrLn "Hello... wait for it..."
      actuate _ x = when x (putStrLn "World!") >> return x
      sense   _   = do
        now      <- getCurrentTime
        lastTime <- readIORef timeRef
        writeIORef timeRef now
        let dt = now `diffUTCTime` lastTime
        return (realToFrac dt, Just ())
  reactimate init sense actuate $ twoSecondsPassed