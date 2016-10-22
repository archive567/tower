> {-# OPTIONS_GHC -Wall #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults #-}
> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
> {-# OPTIONS_GHC -fno-warn-name-shadowing #-}
>
> import Protolude as P
> -- import Data.Maybe (fromJust)
> import Diagrams.Prelude hiding ((<>))
> import Diagrams.TwoD()
> import qualified Diagrams.TwoD.GraphViz as DGV
> import qualified Data.GraphViz as GV
> -- import qualified Data.GraphViz.Attributes.Complete as GV
> import qualified Diagrams.TwoD.Text
> -- import qualified Data.Graph.Inductive.Graph as GI
> import qualified Data.Text as Text
> import qualified Data.Map as Map
> -- import qualified Data.Graph.Inductive.PatriciaTree as FGL
> import Control.Monad.Primitive (unsafeInlineIO)
> import qualified Control.Foldl as L
>
> -- import Tower
> import Diagrams.Backend.SVG (SVG, renderSVG)
> import Diagrams.Backend.Rasterific (Rasterific, renderRasterific)
>
> data Class =
>     Magma |
>     Semigroup |
>     Action |
>     Neutral |
>     Commutative |
>     Invertable |
>     Idempotent |
>     Monoid |
>     Group |
>     Band |
>     Semilattice |
>     Bounded |
>     Lattice |
>     Distributive |
>     Semiring |
>     Ring |
>     Premodule |
>     Semimodule |
>     Module |
>     Division |
>     Field
>     deriving (Show, Eq, Ord)
>
> data Family = Addition | Multiplication deriving (Show, Eq, Ord)
> data Dependency = Dependency { _class :: Class, _dep :: Class, _op :: Maybe Family} deriving (Show, Eq, Ord)
>
>
> dependencies :: [Dependency]
> dependencies = [ Dependency Semigroup Magma Nothing
>       , Dependency Action Magma Nothing
>       , Dependency Neutral Magma Nothing
>       , Dependency Commutative Magma Nothing
>       , Dependency Invertable Neutral Nothing
>       , Dependency Idempotent Magma Nothing
>       , Dependency Monoid Semigroup Nothing
>       , Dependency Monoid Neutral Nothing
>       , Dependency Group Monoid Nothing
>       , Dependency Group Invertable Nothing
>       , Dependency Band Idempotent Nothing
>       , Dependency Band Semigroup Nothing
>       , Dependency Semilattice Band Nothing
>       , Dependency Semilattice Commutative Nothing
>       , Dependency Bounded Semilattice Nothing
>       , Dependency Lattice Semilattice Nothing
>       , Dependency Distributive Magma (Just Addition)
>       , Dependency Distributive Magma (Just Multiplication)
>       , Dependency Semiring Monoid (Just Addition)
>       , Dependency Semiring Monoid (Just Multiplication)
>       , Dependency Semiring Distributive Nothing
>       , Dependency Semiring Commutative (Just Addition)
>       , Dependency Ring Distributive Nothing
>       , Dependency Ring Group (Just Addition)
>       , Dependency Ring Commutative (Just Addition)
>       , Dependency Ring Monoid (Just Multiplication)
>       , Dependency Premodule Semiring Nothing
>       , Dependency Premodule Action Nothing
>       , Dependency Premodule Semigroup Nothing
>       , Dependency Premodule Commutative Nothing
>       , Dependency Semimodule Premodule Nothing
>       , Dependency Semimodule Monoid Nothing
>       , Dependency Semimodule Premodule Nothing
>       , Dependency Semimodule Semiring Nothing
>       , Dependency Module Ring Nothing
>       , Dependency Module Group Nothing
>       , Dependency Module Semimodule Nothing
>       , Dependency Division Ring Nothing
>       , Dependency Division Group (Just Multiplication)
>       , Dependency Field Division Nothing
>       , Dependency Field Commutative (Just Multiplication)
>       ]
>
> fileSvg f s = renderSVG f (mkSizeSpec (Just <$> r2 s))
> filePng f s = renderRasterific f (mkSizeSpec (Just <$> r2 s))


> data Config =
>     Config
>     { _rectX :: Double
>     , _rectY :: Double
>     , _textScale :: Double
>     , _arrowScale :: Double
>     }


-- | Render an annotated graph as a diagram, given functions
--   controlling the drawing of vertices and of edges.  The first
--   function is given the label and location of each vertex. The
--   second function, for each edge, is given the label and location
--   of the first vertex, the label and location of the second vertex,
--   and the label and path corresponding to the edge.

>
>
>
> boxes ps =
>        zipWith (\p c -> place
>                  ((unitSquare #
>                   scaleX 50.0 #
>                   scaleY 25.0 #
>                   lc (sRGB 0.33 0.33 0.33) . opacity 0.3 <>
>                   (Diagrams.Prelude.text (Text.unpack $ show c) # scale 5.0))
>                    # named c) p)
>        (Map.elems ps :: [P2 Double])
>        (Map.keys ps)
>
> edge :: (Renderable (Path V2 Double) b) => Dependency -> QDiagram b V2 Double Any -> QDiagram b V2 Double Any
> edge (Dependency to from Nothing) =
>     connectOutside'
>     (headStyle %~ fc (sRGB 0.33 0.33 0.33) . opacity 0.3 $
>      shaftStyle %~ lc (sRGB 0.33 0.33 0.33) . opacity 0.3 $
>      arrowHead .~ dart $
>      headLength .~ 8 $
>      def)
>     from to
> edge (Dependency to from (Just Addition)) =
>     connectOutside'
>     (headStyle %~ fc red . opacity 0.5 $
>      shaftStyle %~ lc red . opacity 0.5 $
>      arrowHead .~ dart $
>      headLength .~ 8 $
>      def)
>     from to
> edge (Dependency to from (Just Multiplication)) =
>     connectOutside'
>     (headStyle %~ fc blue . opacity 0.5 $
>      shaftStyle %~ lc blue . opacity 0.5 $
>      arrowHead .~ dart $
>      headLength .~ 8 $
>      def)
>     from to
>

> 
>
> ps cs ds = fst $ DGV.getGraph $ unsafeInlineIO $
>     DGV.layoutGraph GV.Dot (DGV.mkGraph cs (toEdge <$> ds))
>   where
>     toEdge (Dependency to from wrapper) = (from,to,wrapper)
>
> instance IsName Class
>
> makeGraph :: ((Renderable (Path V2 Double) b),
>             Renderable (Diagrams.TwoD.Text.Text Double) b) =>
>     Config -> [Class] -> [Dependency] -> (QDiagram b V2 Double Any)
> makeGraph (Config x y s a) cs ds =
>     L.fold (L.Fold (\x a -> edge a x) (mconcat $ boxes (ps cs ds)) identity) ds
>
> classes = [
>     Magma,
>     Semigroup,
>     Action,
>     Neutral,
>     Commutative,
>     Invertable,
>     Idempotent,
>     Monoid,
>     Group,
>     Band,
>     Semilattice,
>     Bounded,
>     Lattice,
>     Distributive,
>     Semiring,
>     Ring,
>     Premodule,
>     Semimodule,
>     Module,
>     Division,
>     Field
>     ]

> main :: IO ()
> main = do
>   let g = makeGraph (Config 3 30 10 1) classes dependencies
>   let g' = makeGraph (Config 3 30 10 1) classes dependencies
>   fileSvg "other/tower.svg" (600,600) (g :: QDiagram SVG V2 Double Any)
>   filePng "other/tower.png" (600,600) (g' :: QDiagram Rasterific V2 Double Any)
