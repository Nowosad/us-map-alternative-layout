# code adapted from https://github.com/clauswilke/dviz.supp/blob/master/data-raw/tidy_census/US_income.R
library(sf)
library(dplyr)
library(spData)
library(tmap)
# helper function to move geometries --------------------------------------
place_geometry = function(geometry, bb, scale_x, scale_y,
                           scale_size = 1) {
  output_geometry = (geometry - st_centroid(geometry)) * scale_size +
    st_sfc(st_point(c(
      bb$xmin + scale_x * (bb$xmax - bb$xmin),
      bb$ymin + scale_y * (bb$ymax - bb$ymin)
    )))
  return(output_geometry)
}

# us data -----------------------------------------------------------------
data("us_states")
data("alaska")
data("hawaii")

# projections -------------------------------------------------------------
# ESRI:102003 https://epsg.io/102003
crs_lower48 = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
# EPSG:3338 https://epsg.io/3338
crs_alaska = "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
# ESRI:102007 https://epsg.io/102007
crs_hawaii = "+proj=aea +lat_1=8 +lat_2=18 +lat_0=13 +lon_0=-157 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# prepare us_lower 48 -----------------------------------------------------
us_lower48 = us_states %>% 
  st_transform(crs_lower48)

# prepare alaska ----------------------------------------------------------
alaska2 = alaska %>% 
  st_transform(crs_alaska) %>% 
  mutate(geometry = place_geometry(geometry, st_bbox(us_lower48), 0.6, 1.4)) %>% 
  st_set_crs(crs_lower48)

# prepare hawaii ----------------------------------------------------------
hawaii2 = hawaii %>% 
  st_transform(crs_hawaii) %>% 
  mutate(geometry = place_geometry(geometry, st_bbox(us_lower48), 0.5, -0.15)) %>% 
  st_set_crs(crs_lower48)

# combine data ------------------------------------------------------------
us_albers_alt = rbind(us_lower48, alaska2, hawaii2)

# plot --------------------------------------------------------------------
tm1 = tm_shape(us_albers_alt) +
  tm_polygons(col = "#0CE878", 
              border.col = "black", lwd = 0.5)

dir.create("figs")
tmap_save(tm1, "figs/us_albers_alt.png",
          height = 1000, width = 824)
# save --------------------------------------------------------------------
dir.create("data")
saveRDS(us_albers_alt, "data/us_albers_alt.rds")
st_write(us_albers_alt, "data/us_albers_alt.gpkg")

dir.create("data/shp")
st_write(us_albers_alt, "data/shp/us_albers_alt.shp")
zip(zipfile = "data/us_albers_alt_shp.zip", files = dir("data/shp", full.names = TRUE), flags = "-j")
