import Model
import Game
import FRP.Yampa

main :: IO ()
main = testMainGameSF

testMainGameSF :: IO ()
testMainGameSF = print $ embed (mainGameSF initGameState) $ deltaEncode 1 $ take 1000 $ repeat actionNone