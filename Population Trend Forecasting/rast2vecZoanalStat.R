# ==============================================================================
# SOURCE: https://tmieno2.github.io/R-as-GIS-for-Economists/int-RV.html
# ==============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  terra, # handle raster data
  raster, # handle raster data
  exactextractr, # fast extractions
  sf, # vector data operations
  dplyr, # data wrangling
  tidyr, # data wrangling
  data.table, # data wrangling
  # prism, # download PRISM data
  # tictoc, # timing codes
  # tigris, # to get county sf
  tmap # for mapping
)
library("exactextractr")

# temp <- tempfile()
# # Unzip the contents of the temp and save unzipped content in 'temp2'
# unzip(zipfile ='D:/Work/Box Sync/Trends_all states/Maps_2020/tl_2020_06_place.zip', exdir = temp)



files <- Sys.glob(file.path('D:/Work/Box Sync/Spatial_Population_geotiff/SSP1_1km/', "ssp1_total*.tif")) 

for (file in files){
  raster <- rast(file)
  
  # Read the shapefile. Alternatively make an assignment, such as f<-sf::read_sf(your_SHP_file)
  poly <- sf::read_sf('D:/Work/Box Sync/Trends_all states/Maps_2020/compiled.shp') %>%
    #--- transform using the CRS of the raster data  ---#
    st_transform(terra::crs(raster))
  
  # convert sf object to a SpatVector.
  #--- City boundary (SpatVector) ---#
  ploy_sv <- vect(poly)
  
  #--- crop the entire raster by vector boundary---#
  raster_sr <- terra::crop(raster, poly)
  
    #--- extract values from the raster for each county ---#
  popByCity <-
    exact_extract(
      raster_sr,
      poly,
      c('mean','stdev', 'sum'),
      # using fucntion name rather than the following lines
      # function(values, coverage_fraction)
      # sum(values * coverage_fraction, na.rm=TRUE),
      # #--- this is for not displaying progress bar ---#
      progress = FALSE
    )  
  
  
  #--- take a look at the first 6 rows of the first two list elements ---#
  popByCity[1:3] %>% lapply(function(x) head(x))
  
  poly$population <- popByCity$sum
  poly$meanpop <- popByCity$mean
  poly$std <- popByCity$stdev
  
  poly <- st_drop_geometry(poly)
  colnames(poly)
  filename <- strsplit(file, "1km//")[[1]][2]
  poly %>%
    select(GEOID, NAME, NAMELSAD, population, meanpop, std) %>% 
    write.csv(paste0("D:/Work/Box Sync/Spatial_Population_geotiff/Population from SSP/r_extract_output//", filename, ".csv"),
              row.names = FALSE)

}





# ===========================================================================================
raster <- rast('D:/Work/Box Sync/Spatial_Population_geotiff/SSP2_1km/ssp2_total_2020.tif')

# temp <- tempfile()
# # Unzip the contents of the temp and save unzipped content in 'temp2'
# unzip(zipfile ='D:/Work/Box Sync/Trends_all states/Maps_2020/tl_2020_06_place.zip', exdir = temp)

# Read the shapefile. Alternatively make an assignment, such as f<-sf::read_sf(your_SHP_file)
poly <- sf::read_sf('D:/Work/Box Sync/Trends_all states/Maps_2020/compiled.shp') %>%
  #--- transform using the CRS of the PRISM tmax data  ---#
  st_transform(terra::crs(raster))

# convert KS_county_sf (an sf object) to a SpatVector.
#--- Kansas boundary (SpatVector) ---#
ploy_sv <- vect(poly)

#--- crop the entire PRISM to its KS portion---#
raster_sr <- terra::crop(raster, poly)


# #--- extract values from the raster for each county ---#
# # using terra
# popByCity <- terra::extract(x =raster_sr, y =poly_sv, exact = TRUE)
# popByCity %>%
#   group_by(ID) %>%
#   summarize(
#     populationData = sum(fraction * ssp2_total_2020)
#   )

# head(popByCity, 20)


library("exactextractr")

#--- extract values from the raster for each county ---#
popByCity <-
  exact_extract(
    raster_sr,
    poly,
    c('mean','stdev',  'sum'),
    # using fucntion name rather than the following lines
    # function(values, coverage_fraction)
    # sum(values * coverage_fraction, na.rm=TRUE),
    # #--- this is for not displaying progress bar ---#
    progress = FALSE
  )  


#--- take a look at the first 6 rows of the first two list elements ---#
popByCity[1:3] %>% lapply(function(x) head(x))

poly$population <- popByCity$sum
poly$meanpop <- popByCity$mean

poly <- st_drop_geometry(poly)
colnames(poly)
poly %>%
  select(GEOID, NAME, NAMELSAD, population, meanpop) %>% 
  write.csv(file = "D:/Work/Box Sync/Spatial_Population_geotiff/Population from SSP/populationByCity.csv",
            row.names = FALSE)


poly[grep("Industry", poly$NAME), ]$population


# 
# 
# sp::plot(raster_sr, axes = FALSE)
# sp::plot(poly, add = TRUE)
