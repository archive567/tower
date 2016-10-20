
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
> vs = [0..29]
>
> ls :: [Text]
> ls = [ "Semigroup"
>      , "Actor"
>      , "Scalar"
>      , "Monoid"
>      , "Cancellative"
>      , "Group"
>      , "Abelian"
>      , "Times"
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
>      , "TensorAlgebra"
>      , "Hilbert"
>      ]
>
>      
> es :: [(Int,Int,Text)]
> es = [ (0, 1, "")
>      , (13, 2, "")
>      , (0, 3, "")
>      , (0, 4, "")
>      , (0, 5, "")
>      , (3, 6, "")
>      , (3, 7, "")
>      , (6, 7, "")
>      , (3, 8, "")
>      , (7, 8, "")
>      , (5, 9, "")
>      , (8, 9, "")
>      , (9, 10, "")
>      , (9, 11, "")
>      , (2, 13, "")
>      , (9, 13, "")
>      , (11, 14, "")
>      , (12, 14, "")
>      , (13, 14, "")
>      , (14, 16, "")
>      , (15, 16, "")
>      , (9, 17, "")
>      , (10, 17, "")
>      , (9, 18, "")
>      , (11, 19, "")
>      , (18, 19, "")
>      , (19, 20, "")
>      , (6, 21, "")
>      , (21, 22, "")
>      , (22, 23, "")
>      , (2, 24, "")
>      , (11, 24, "")
>      , (21, 24, "")
>      , (2, 26, "")
>      , (12, 26, "")
>      , (25, 26, "")
>      , (13, 27, "")
>      , (24, 27, "")
>      , (2, 28, "")
>      , (11, 28, "")
>      , (13, 28, "")
>      , (24, 28, "")
>      , (2, 29, "")
>      , (5, 29, "")
>      , (14, 29, "")
>      , (20, 29, "")
>      , (27, 29, "")
>      , (28, 29, "")
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
>       , "cancel"
>      ]
