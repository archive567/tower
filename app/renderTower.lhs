
> {-# OPTIONS_GHC -Wall #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults #-}
> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
> import Protolude as P
> import Data.Maybe (fromJust)
> import Diagrams.Prelude hiding ((<>))
> import Diagrams.TwoD()
> import qualified Diagrams.TwoD.GraphViz as DGV
> import qualified Data.GraphViz as GV
> import qualified Data.GraphViz.Attributes.Complete as GV
> import qualified Diagrams.TwoD.Text
> import qualified Data.Graph.Inductive.Graph as GI
> import qualified Data.Text as Text
> import qualified Data.Map as Map
> import Data.List
>
> -- import Tower
> import Diagrams.Backend.SVG (SVG, renderSVG)
> import Diagrams.Backend.Rasterific (Rasterific, renderRasterific)

> vs :: [Int]
> vs = [0..31]
>
> ls :: [Text]
> ls = [ "Semigroup"
>      , "Actor"
>      , "Action"
>      , "Scalar"
>      , "Monoid"
>      , "Cancellative"
>      , "Group"
>      , "Abelian"
>      , "Rg"
>      , "Semiring"
>      , "Ring"
>      , "Integral"
>      , "Field"
>      , "Ord"
>      , "Normed"
>      , "OrdField"
>      , "Bounded"
>      , "BoundedField"
>      , "QuotientField"
>      , "ExpRing"
>      , "ExpField"
>      , "Real"
>      , "Module"
>      , "FreeModule"
>      , "FiniteModule"
>      , "VectorSpace"
>      , "Eq"
>      , "Metric"
>      , "Banach"
>      , "Hilbert"
>      , "TensorAlgebra"
>      ]
>
> ls' :: [Text]
> ls' = [ "mappend"
>       , "Actor"
>       , ".+"
>       , "Scalar"
>       , "mempty"
>       , "-"
>       , "negate"
>       , "+, zero"
>       , "*"
>       , "one"
>       , "Ring"
>       , "div, mod, toInteger, rem, quot"
>       , "/"
>       , "<, >"
>       , "size"
>       , "minBound, MaxBound"
>       , "nan, infinity"
>       , "round"
>       , "**"
>       , "sqrt, log, exp"
>       , "pi, trig"
>       , ".*"
>       , ".*."
>       , "dim, unsafeToModule"
>       , "./, ./."
>       , "Eq"
>       , "distance"
>       , "normalize"
>       , "><"
>       , "<?>"
>      ]
>      
> es :: [(Int,Int,Text)]
> es = [ (0, 1, "")
>      , (0, 2, "")
>      , (2, 3, "")
>      , (1, 3, "")
>      , (0, 4, "")
>      , (1, 5, "")
>      , (4, 5, "")
>      , (1, 6, "")
>      , (5, 6, "")
>      , (3, 7, "")
>      , (5, 7, "")
>      , (6, 8, "")
>      , (7, 8, "")
>      , (0, 10, "")
>      , (9, 10, "")
>      , (8, 11, "")
>      , (8, 12, "")
>      , (8, 15, "")
>      , (13, 15, "")
>      , (14, 15, "")
>      , (14, 16, "")
>      , (15, 16, "")
>      , (12, 16, "")
>      , (12, 18, "")
>      , (17, 18, "")
>      , (12, 19, "")
>      , (11, 19, "")
>      , (8, 20, "")
>      , (12, 21, "")
>      , (20, 21, "")
>      , (21, 22, "")
>      , ( 3, 23, "")
>      , ( 4, 23, "")
>      , (13, 23, "")
>      , (23, 24, "")
>      , (24, 25, "")
>      , (24, 26, "")
>      , (12, 26, "")
>      , (13, 26, "")
>      , (27, 28, "")
>      , (13, 28, "")
>      , (26, 29, "")
>      , (28, 29, "")
>      , (15, 29, "")
>      , (22, 30, "")
>      , (16, 30, "")
>      , (31, 30, "")
>      , (13, 30, "")
>      , (29, 30, "")
>      , (13, 31, "")
>      , (12, 31, "")
>      , (15, 31, "")
>      , (26, 31, "")
>      ]
>
> fileSvg f s = renderSVG f (mkSizeSpec (Just <$> r2 s))
> filePng f s = renderRasterific f (mkSizeSpec (Just <$> r2 s))

> makeQ :: ((Renderable (Diagrams.TwoD.Text.Text Double) b), Renderable (Diagrams.Prelude.Path V2 Double) b)
>      => [Int] -> [Text] -> [(Int,Int,Text)] -> IO (QDiagram b V2 Double Any)
> makeQ vs' ls' es' = do
>   gr <- DGV.layoutGraph GV.Dot (DGV.mkGraph vs' es')
>   let n = GI.labNodes gr
>       vmap = Map.fromList [ (i, pt) | (i,(attrs,_)) <- n, GV.Pos (GV.PointPos pt) <- attrs ]
>       v2 i = (\(GV.Point x y _ _) -> V2 x y) . fromJust $ (Map.lookup i vmap)
>       edgeLengths = [ v2 i `distance` v2 j
>                     | (i, j, _) <- GI.labEdges gr
>                     ]
>       nodeRadius = minimum edgeLengths / 4
>       drawing = DGV.drawGraph
>                      (\ i p -> place (((circle nodeRadius # lc grey) <> (Diagrams.Prelude.alignedText 0 0 (Text.unpack $ ls' !! i)) # scale 10)) p )
>                      (\ _ p₁ _ p₂ _ p -> arrowBetween' (opts p) p₁ p₂ # lc grey)
>                      gr
>       opts p = with P.& gaps .~ Diagrams.Prelude.local nodeRadius
>                     P.& arrowShaft .~ (unLoc . fromJust $ P.head $ pathTrails p)
>                     P.& headLength .~ Diagrams.Prelude.local nodeRadius
>   return drawing

> main :: IO ()
> main = do
>   g <- makeQ vs ls es
>   g' <- makeQ vs ls es
>   fileSvg "other/tower.svg" (400,600) (g :: QDiagram SVG V2 Double Any)
>   filePng "other/tower.png" (400,600) (g' :: QDiagram Rasterific V2 Double Any)

