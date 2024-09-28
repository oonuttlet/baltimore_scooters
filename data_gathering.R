library(mapbaltimore)
library(tidyverse)


balt_b <- mapbaltimore::baltimore_city_detailed %>%#load in baltimore shape from mapbaltimore
  st_transform(4326) %>%
  st_make_valid()

fbs <- list.files("data", recursive = TRUE, full.names = TRUE)

fbs_raw <- lapply(fbs, jsonlite::fromJSON)
fbs_df <- lapply(fbs_raw, function(x) bikes <- x$data$bikes %>%
                   select('bike_id','lat','lon'))
fbs_bikes_bind <- as.data.frame(do.call(rbind,fbs_df))
fbs_gdf <- st_as_sf(fbs_bikes_bind, coords = c('lon', 'lat'), crs = 4326) %>%
  st_transform(3857)
plot(st_geometry(fbs_gdf)) #check for distribution

balt_linear <- balt_b %>% st_transform(3857)
hex_linear <- balt_linear %>% st_make_grid(cellsize = 200, what = 'polygons', square = FALSE)
hex_linear.sub <- st_sf(hex_linear[balt_linear]) %>%
  mutate(hex_id = row_number())
plot(hex_linear.sub)

sph <- st_join(fbs_gdf, hex_linear.sub, left = FALSE) %>%
  count(hex_id) %>% st_drop_geometry()
hex_out <- left_join(hex_linear.sub, sph, by = "hex_id") %>%
  replace(is.na(.), 0) %>% rename("geometry" = 3)

st_write(hex_out, "doc/hex_out.gpkg")
tm_shape(hex_out)+
  tm_polygons(col = 'n', breaks = seq.int(0, 4500, by = 500))
