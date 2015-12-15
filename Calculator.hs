-- Part 2 of the lab
-- Author: Johan Gustafsson

import Control.Monad (when)
import Data.Maybe
import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Pages

import Expr


canWidth  = 300
canHeight = 300

-- Main function of the haste module
main = do
    -- Elements
    canvas        <- mkCanvas canWidth canHeight
    zoomTxt       <- mkHTML "<b>Zoom scalar</b>"
    zoomInput     <- mkInput 20 "0.04" 
    fx            <- mkHTML "<i>f</i>(<i>x</i>)="
    input         <- mkInput 20 "sin(x)"
    draw          <- mkButton "Draw graph"
    differ        <- mkButton "Differentiate"
    diffOutput    <- mkInput 20 ""

    -- Layout
    formula <- mkDiv
    row formula [fx,input]
    zoomCtrl <- mkDiv
    row zoomCtrl [zoomTxt,zoomInput]
    column documentBody [canvas,zoomCtrl,formula,draw,differ,diffOutput]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    -- Interaction
    Just can <- fromElem canvas
    onEvent draw  Click $ \_      -> readAndDraw zoomInput input can
    onEvent input KeyUp $ \code   -> when (code==13) $ readAndDraw zoomInput input can
    onEvent differ  Click $ \_    -> differAndOutput input diffOutput


-- Returns all points of the given expressions
points :: Expr -> Double -> (Int,Int) -> [Point]
points e s (w,h) = ps
        where ps = [ (x/s, getY x) | x <- [-150..150] ]
              getY v = (negate (eval e v)/s) + ((fromIntegral h)/2)

readAndDraw :: Elem -> Elem -> Canvas -> IO ()
readAndDraw zoom ele c = do
            scaling <- getProp zoom "value"
            text <- getProp ele "value"
            render c (stroke $ path $ points (fromJust $ readExpr text) (read scaling :: Double) (canWidth, canHeight) )

differAndOutput :: Elem -> Elem -> IO ()
differAndOutput exp output = do 
                text <- getProp exp "value"
                setProp output "value" ((showExpr $ readAndDiff text))