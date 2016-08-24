import Model
import Game
import FRP.Yampa

main :: IO ()
main = testMainGameSF

testMainGameSF :: IO ()
testMainGameSF = mapM_ (print . map objectPosVel) $ embed (mainGameSF initGameState) $ deltaEncode 0.01 $ take 1000 $ repeat actionNone

objectPosVel :: GameObject -> String
objectPosVel ob1 = " | " ++ show (objectType ob1)++ ", Attributes: " ++show (pos ob1)++" "++show (vel ob1)++" rota: "++show (rot ob1) ++ " |"
