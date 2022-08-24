library(sf); library(tidyverse); library(readxl); library(stringr); library(mapview);
library(elevatr); library(terra); library(wopr); library(osmdata); library(exactextractr)

load("data/processed/0_gps_points.rda"); mapview(points)

st_write(points, "data/processed/gps_points.shp")

#'----------------------------------------
#' * elevation *
#' https://github.com/jhollist/elevatr

elevation <- get_elev_raster(points, z=9, src = "aws"); plot(elevation) # zoom at ~ 200m pixel

# correct crs
points <- st_transform(points, crs=3857); crs <- st_crs(points); mapview(points)
points$zone_id <- as.factor(points$zone_id)

#'----------------------------------------
#' * climate * 
#' https://data.ceda.ac.uk/badc/ukmo-hadobs/data/insitu/MOHC/HadOBS/HadUK-Grid/v1.1.0.0/1km

temperature <- terra::rast("data/raw/tas_hadukgrid_uk_1km_ann_202101-202112.nc"); plot(temperature)
rainfall <- terra::rast("data/raw/rainfall_hadukgrid_uk_1km_ann_202101-202112.nc"); plot(rainfall)
terra::crs(temperature)

#'----------------------------------------
#' * human density *
#' #' https://www.data.gov.uk/dataset/ca2daae8-8f36-4279-b15d-78b0463c61db/uk-gridded-population-2011-based-on-census-2011-and-land-cover-map-2015

population <- terra::rast("data/raw/population_workday_census.tif"); plot(population)

#'----------------------------------------
#' * greenspace density *
#' https://osdatahub.os.uk/downloads/open/OpenGreenspace

greenspace <- read_sf("data/raw/greenspace_subset.shp")
 greenspace <- st_transform(greenspace, crs); st_crs(greenspace)

#'----------------------------------------
#' * land cover *
#' 10m, 2020, CEH land cover raster

lc <- terra::rast("Y:\\extra_data\\uk_2020_lc\\gb2020lcm10m.tif"); plot(lc); lc <- lc[[1]]

#'----------------------------------------
#' * road density *
#' OSM, https://download.geofabrik.de/, highway class

roads <- read_sf("data/raw/road_network_subset.shp")
  roads <- st_transform(roads, crs); st_crs(roads)

#'----------------------------------------
#' * extract variables *

buffer_sizes <- c(100, 500, 1000, 3500, 7000) # meters 

for(b in buffer_sizes) {
  
  print(paste0("buffer radius = ", b))
  
  data <- st_buffer(points, dist=b)
  
  # raster extracts of mean values in buffer
  data$temperature <- exact_extract(temperature, data, fun = "mean")
  data$rainfall <- exact_extract(rainfall, data, fun = "mean")
  data$elevation <- exact_extract(elevation, data, fun = "mean") 
  data$population <- exact_extract(population, data, fun = "mean") 

  # get area of greenspace and ratio to buffer
  t <- st_intersection(data, greenspace) %>% 
    mutate(area=st_area(.)) %>% 
    group_by(zone_id) %>% summarise(green_area = as.numeric(sum(area))) %>% as_tibble %>% select(zone_id, green_area)
  data <- left_join(data, t, by="zone_id"); data$green_area[is.na(data$green_area)] <- 0
  data$green_density <- data$green_area/(b*b*3.14) # m^2/m^2
  
  # get road length and density
  t <- st_intersection(data, roads) %>% 
    mutate(value=st_length(.)) %>% 
    group_by(zone_id) %>% summarise(road_length = as.numeric(sum(value))) %>% as_tibble %>% select(zone_id, road_length) 
  data <- left_join(data, t, by="zone_id"); data$road_length[is.na(data$road_length)] <- 0
  data$road_density <- data$road_length/(b*b*3.14) # m/m^2
  
  # land cover extract
  t <- exact_extract(lc, data, max_cells_in_memory = 3e+08)
  t <- lapply(t, function(x){ x %>% select(value) %>% c() %>% unlist()})

  #' calculate *% of urban land cover*, class = 20
  data$urban <- unlist(lapply(t, function(x){ (sum(x==20 | x==21, na.rm = T)/length(x)) })) * 100
  
  #' calculate *% of forest land cover*, class = 1(broad leaved) + 2(coniferous) 
  data$forest <- unlist(lapply(t, function(x){ (sum(x==1 | x==2, na.rm = T)/length(x)) })) * 100
 
  #' calculate *% of grassland cover*, class = 52(Shrub) + 71(Grassland/Herbaceous) + 81(Pasture/Hay)
  data$grass <- unlist(lapply(t, function(x){ (sum(x==4 | x==5 | x==6 | x==7 | x==9 | x==10, na.rm = T)/length(x)) })) * 100
  
  #' calculate *% of cropland cover*,
  data$crop <- unlist(lapply(t, function(x){ (sum(x==3, na.rm = T)/length(x)) })) * 100
  
  #' calculate *% of wetland cover*
  data$wet <- unlist(lapply(t, function(x){ sum(x==11 | x==8 | x==13 | x==14 | x==15 | x==16 | x==17 | x==18 | x==19 | x==12
                                                            , na.rm = T)/length(x) })) *100
 
  #' calculate *land cover diversity*, as q=0 or species richness.
  data$lc_q0 <- unlist(lapply(t, function(x){ length(unique(x))}))
  
  #' calculate *landscape heterogeneity/diversity*, as q=1 or Shannon entropy measure.
  #' Total number of different entities (in this case land covers) when accounting for their abundance
  #' i.e. Effective number of land cover
  data$lc_q1 <- unlist(lapply(t, 
                                  
                                  function(x){ # get vector of abundance of each land cover
                                    abundance.vector <- tibble(lc = as.factor(x)) %>% count(lc) %>% dplyr::select(n)
                                    
                                    # calculate ratio of each lc category within total n of pixels
                                    ratio.vector <- as.vector(t(abundance.vector/sum(abundance.vector)))
                                    
                                    # get Shannon entropy as effective number of species, q=1
                                    exp(-sum(ratio.vector * log(ratio.vector)))}))
  
  data$rainfall[data$zone_id == "139"] <- 694.53; data$temperature[data$zone_id == "139"] <- 10.34
  data$rainfall[data$zone_id == "43"] <- 642.42; data$rainfall[data$zone_id == "43"] <- 10.37
  
  save(data, file = paste0("data/processed/landscape_data_r", b, ".rda"))
  write_csv(data %>% st_drop_geometry(), file = paste0("data/processed/landscape_data_r", b, ".csv"))

}
