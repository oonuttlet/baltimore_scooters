# baltimore_scooters #
# harrison deford -- @oonuttlet -- hdeford1@umbc.edu #
# this script gathers micromobility data in the gbfs format,
# then bins it to hexes.

library(sf)
library(mapbaltimore)
library(tidyverse)
library(httr)
#library(tmap)
library(MetBrewer)
library(colorspace)
library(stars)
library(rayshader)
library(rayrender)
library(glue)

map <- "baltimore_scooters"

hex_in <- st_read("doc/hex_out.gpkg")
bb <- st_bbox(hex_in)
yind <- st_distance(st_point(c(bb[["xmin"]], bb[["ymin"]])), 
                    st_point(c(bb[["xmin"]], bb[["ymax"]])))
xind <- st_distance(st_point(c(bb[["xmin"]], bb[["ymin"]])), 
                    st_point(c(bb[["xmax"]], bb[["ymin"]])))

if (yind > xind) {
  y_rat <- 1
  x_rat <- xind / yind
} else {
  x_rat <- 1
  y_rat <- yind / xind
}

pal <- "baltimore_scooters"
c1 <- met.brewer("Hiroshige")
colors <- c1[c(5:1, 10:6)] |> rev()
swatchplot(colors)

#colors <- c('#000000','#EAAA00')
texture <- grDevices::colorRampPalette(colors, bias = 2)(256)
size <- 3000
rast <- st_rasterize(hex_in %>% dplyr::select(n, geom),
                     nx = floor(size*x_rat), ny = floor(size*y_rat))
#plot(rast)

mat <- matrix(rast$n, nrow = floor(size * x_rat), ncol = floor(size * y_rat))

try(rgl::rgl.close())

mat %>%
  height_shade(texture = texture) %>%
  plot_3d(heightmap = mat,
          zscale = 3,
          solid = FALSE,
          soliddepth = 0,
          windowsize = c(800,800),
          phi = 61.9623200,
          zoom = 1,
          theta = -16.4382620,
          background = "white")

outfile <- paste0("bin/", map, ".png")
outfile
if(!file.exists(outfile)){
  png::writePNG(matrix(1), outfile)
}

saveRDS(list(
  map = map,
  pal = pal,
  colors = colors,
  outfile = outfile,
  coords = coords
), glue("header.rds"))

render_highquality(
  # We test-wrote to this file above, so we know it's good
  outfile, 
  # See rayrender::render_scene for more info, but best
  # sample method ('sobol') works best with values over 256
  samples = 400, 
  preview = TRUE,
  light = TRUE,
  lightdirection = rev(c(-120, -120, -130, -130)),
  lightcolor = c(colors[3], "white", colors[7], "white"),
  lightintensity = c(650, 50, 800, 50),
  lightaltitude = c(30, 60, 30, 60),
  # All it takes is accidentally interacting with a render that takes
  # hours in total to decide you NEVER want it interactive
  interactive = FALSE,
  #HDR lighting used to light the scene
  # environment_light = "doc/phalzer_forest_01_4k.hdr",
  # environment_light = "assets/env/small_rural_road_4k.hdr",
  # Adjust this value to brighten or darken lighting
  # intensity_env = 1.5,
  # Rotate the light -- positive values move it counter-clockwise
  # rotate_env = 130,
  # This effectively sets the resolution of the final graphic,
  # because you increase the number of pixels here.
  # width = round(6000 * wr), height = round(6000 * hr),
  width = 4000, height = 4000
)




