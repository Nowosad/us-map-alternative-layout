Alternative layout for maps of the United States
================

[![DOI](https://zenodo.org/badge/161046120.svg)](https://zenodo.org/badge/latestdoi/161046120)

<img src="figs/us_albers_alt.png" height="400" style="display: block; margin: auto;" />

Maps of the United States often focus only on the contiguous 48 states.
In many maps, Alaska and Hawaii are simply not shown or are displayed at
different geographic scales than the main map. This repo contains [an R
code](https://github.com/Nowosad/us-map-alternative-layout/blob/master/R/01_create_alternative_layout.R)
to create an inset map of the USA, which shows all its states and
ensures relative sizes are preserved.

## Data

Spatial data allowing for alternative visualizations of the United
States is available in:

  - [GeoPackage
    format](https://github.com/Nowosad/us-map-alternative-layout/raw/master/data/us_albers_alt.gpkg)
  - [ESRI Shapefile
    format](https://github.com/Nowosad/us-map-alternative-layout/raw/master/data/us_albers_alt_shp.zip)
  - [RDS
    format](https://github.com/Nowosad/us-map-alternative-layout/raw/master/data/us_albers_alt.rds)

## Acknowledgements

The code for map layout creation was adapted from the visualizing
geospatial data chapter of [the Fundamentals of Data Visualization
book](https://serialmentor.com/dataviz/geospatial-data.html) by [Claus
Wilke](https://twitter.com/ClausWilke) and builds upon an idea from a
blog post - [Making maps of the USA with R: alternative
layout](https://nowosad.github.io/post/making-alternative-inset-maps-of-the-usa/).

Read [the Geocomputation with R
book](https://geocompr.robinlovelace.net/) to learn more about spatial
data operations and visualizations.
