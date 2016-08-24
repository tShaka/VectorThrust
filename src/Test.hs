import Model
import Game
import FRP.Yampa

main :: IO ()
main = testMainGameSF

testMainGameSF :: IO ()
testMainGameSF = mapM_ (print . format ) $ embed (mainGameSF initGameState) $ deltaEncode 0.01 $ take 100 $ repeat actionNone
    where format (objects,collisions) = (map objectPosVel objects, collisions)

objectPosVel :: GameObject -> String
objectPosVel ob1 = " | " ++ show (objectType ob1)++ ", Attributes: " ++show (pos ob1)++" "++show (vel ob1)++" rota: "++show (rot ob1) ++ " | COL? :"
