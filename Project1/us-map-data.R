# downloaded shape from file from:http://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
# gz_2010_us_040_00_20m.zip

library(mapproj)
library(maptools)
library(dplyr)
library(ggplot2)

usa <- readShapeSpatial("gz_2010_us_040_00_20m//gz_2010_us_040_00_20m.shp")
usa_df <- fortify(usa)
usa_df$state <-  as.character(usa$NAME[as.numeric(usa_df$id) + 1])

usa_df <- subset(usa_df, state != "Puerto Rico")
# remove Puerto Rico 

# wrap alutiean?
usa_df$long[usa_df$long  > 0] <- usa_df$long[usa_df$long  > 0] - 360

# put alaska and hawaii in different facets
usa_df$ak_hi <- as.character(usa_df$state)
usa_df$ak_hi[!(usa_df$ak_hi %in% c("Alaska", "Hawaii"))] <- "lower48"

# add abbreviations
abbs <- tolower(c(state.abb, "dc"))
names(abbs) <- c(state.name, "District of Columbia")

usa_df$state.abb <- abbs[usa_df$state]
usa_df <- plyr::rename(usa_df, 
  c("state" = "state_name", "state.abb" = "state"))

# long, lat version with alaska and hawaii in correct locations
saveRDS(usa_df, "usa-state-map.rds")

# == project and move ak and hi == #
# all 
lower48 <- subset(usa_df, 
  !(state_name %in% c("Alaska", "Hawaii")))

ak <- subset(usa_df, state_name == "Alaska")
hi <- subset(usa_df, state_name == "Hawaii")
# scale and move closer


ak_proj <- as.data.frame(mapproject(ak$long,  ak$lat,
  projection = "albers", 
  parameters = list(lat0 = 55, lat1 = 65))[c("x","y")])
ak_proj <- cbind(ak, ak_proj)

hi_proj <- as.data.frame(mapproject(hi$long,  hi$lat,
  projection = "albers", 
  parameters = list(lat0 = 8, lat1 = 18))[c("x","y")])
hi_proj <- cbind(hi, hi_proj)

lower48_proj <- as.data.frame(
  mapproject(lower48$long, lower48$lat,
    projection = "albers", 
    parameters = list(lat0 = 29.5, lat1 = 45.5))[c("x","y")])
lower48_proj <- cbind(lower48, lower48_proj)

ak_proj_trans <- mutate(ak_proj,
  x = 0.35*x - 0.4, y = 0.35*y - 1.25)
hi_proj_trans <- mutate(hi_proj,
  x = x - 0.2, y = y + 2.7)

usa_all <- rbind(lower48_proj, hi_proj_trans, ak_proj_trans)
# x, y version with alaska and hawaii near lower 48
saveRDS(usa_all, "usa-state-map_all.rds")
