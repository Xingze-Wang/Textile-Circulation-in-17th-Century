library(tidyverse)
library(geojsonio)
library(leaflet)
library(magrittr)
library(raster)
library(sf)

# get everything under geoJSON/
jsons <- list.files(
  path = "geoJSON",
  pattern = "*",
  recursive = FALSE,
  full.names = TRUE
)

#### read geojson ####
# use leaflet()
# not ggplot-able, needs fortifying
# https://www.r-graph-gallery.com/327-chloropleth-map-from-geojson-with-ggplot2.html

# cannot_read: missing name data
# Error in h(simpleError(msg, call)) : error in
# evaluating the argument 'x' in selecting a method
# for function 'addAttrToGeom': conversion from
# feature type sfc_GEOMETRY to sp is not supported

# cannot_join: duplicate subscripts for columns

##### with loop #####
for (i in 1:length(jsons)) {
  print(i)
  if (i == 1) {
    geo <- jsons[i] %>%
      as.location() %>%
      geojson_read(what = "sp")
    name <- basename(jsons[i]) %>%
      # remove extension
      tools::file_path_sans_ext()
    geo@data <- geo@data %>%
      mutate(region = name)
    joined <- geo
    
  } else {
    print(jsons[i])
    geo <- jsons[i] %>%
      as.location() %>%
      geojson_read(what = "sp")
    name <- basename(jsons[i]) %>%
      # remove extension
      tools::file_path_sans_ext()
    geo@data <- geo@data %>%
      mutate(region = name)
    joined %<>%
      raster::union(geo)
  }
}

# dump NAs
# joined@data <- joined@data %>%
joined@data %<>%
  mutate(region = coalesce(
    region, region.1, region.2
  ))

# call it a day
joined %>%
  saveRDS("hist_geo.rds")

##### without loop #####
# no need to run this
# for (i in jsons){
#   print(i)
#   geo <- geojson_read(as.location(i), what = "sp")
#   assign(tools::file_path_sans_ext(basename(i)), geo)
# }

#### Just try a plot ####
joined %>%
  leaflet() %>%
  addPolygons(
    color = "black",
    label = ~NAME_0
  ) %>%
  addTiles()

# great, view data
View(joined@data)

#### read_sf() ####
# cannot seem to bind data
# need to create variables & full join
# can be ggplot-ted
#
# for (i in jsons){
#   # for debugging
#   # print(i)
#   # read as sf
#   # see below for geojson_read() issues
#   geo <- read_sf(as.location(i))
#   # sanitize name: remove dirs
#   name <- basename(i) %>%
#     # remove extension
#     tools::file_path_sans_ext() %>%
#     # replace specials with "_"
#     str_replace_all("[^[:alnum:]]", "_")
#   # assign
#   assign(name, geo)
# }
#
# # tried: st_join(), full_join(), rbind(), cbind(),
# # st_bind_cols(), st_union()
# zzjoin <- st_join(Ardra,
#                   Arguin,
#                   join = st_nearest_feature,
#                   left = TRUE)
