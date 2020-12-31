{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Internal.Plot
  ( plot,
  )
where

--------------------------------------------------------------------------
import Internal.Graph
import Internal.Statistics
import Prelude hiding (lines)
import Data.Functor
import Graphics.Rendering.Chart.Easy hiding (plot)
import qualified Graphics.Rendering.Chart.Backend.Diagrams as Backend
import qualified Data.List as List

--------------------------------------------------------------------------

-- | Plots the results of the experiment in a line chart in the given file.
--
-- The chart includes the standard deviation of the multiple rands
plot :: FilePath -> [(Int, Statistics Weight)] -> IO ()
plot fp values = toFile (baseName fp ++ ".svg") layout
  where
    layout = layout_title .~ ("Minimum Spanning Tree: weight ~ vertices")
           -- layout_margin .~ 20
           $ layout_plots .~ [toPlot lines, toPlot errBars]
           $ layout_x_axis . laxis_title .~ "Number of vertices"
           -- layout_x_axis . laxis_style . axis_label_gap .~ 1
           $ layout_y_axis . laxis_title .~ "Weight"
           $ def

    lines :: PlotLines Int Weight
    lines = plot_lines_style . line_color .~ opaque red
           $ plot_lines_style . line_width .~ 1.5
           $ plot_lines_title .~ "mean"
           $ plot_lines_values .~ [ [ (vertices, _mean)
                                  | (vertices, Statistics{..}) <- values] ]
           $ def

    errBars :: PlotErrBars Int Weight
    errBars = plot_errbars_values .~ [symErrPoint vertices _mean dx dy
                                     | (vertices, Statistics{..}) <- values
                                     , let dx = 0
                                           dy = if isNaN _std then 0.0 else _std
                                     ]
             $ plot_errbars_line_style . line_color .~ blue `withOpacity` 0.5
             $ plot_errbars_title .~ "std"
             $ def

-- | Renders a plot in the given file using Diagrams as a backend.
toFile :: (ToRenderable a) => FilePath -> a -> IO ()
toFile fp =
  void . Backend.renderableToFile def fp . toRenderable

-- | Returns the basename of a file.
--
-- >>> basename "foo.png"
-- "foo"
baseName :: FilePath -> String
baseName = fst . List.span (/= '.')
