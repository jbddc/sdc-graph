import Diagrams.Prelude
import Diagrams.TwoD.Arrow 

main = mainWith (example :: Diagram B)

node :: Int -> Diagram B
node n = text (show n) # fontSizeL 0.2 # fc white
      <> circle 0.2 # fc green # named n

arrowOpts = with & gaps       .~ small
                 & headLength .~ local 0.15

tournament :: Int -> Diagram B
tournament n = atPoints (trailVertices $ regPoly n 1) (map node [1..n])
  # applyAll [connectOutside' arrowOpts j k | j <- [1 .. n-1], k <- [j+1 .. n]]

example = tournament 6
