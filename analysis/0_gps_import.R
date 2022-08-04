library(sf); library(tidyverse); library(readxl); library(stringr); library(mapview)

gps <-  read_excel("data/raw/gps_points.xlsx")

temp <- as.data.frame(str_split(gps$`GPS Coordinates`, " "))

gps$y <- as.character(temp[1,]) %>% str_replace(",", "") %>% str_pad(width = 10, side = "right", pad = 0)
gps$x <- as.character(temp[2,]) %>% str_replace(",", "") %>% str_pad(width = 10, side = "right", pad = 0)

points <- st_as_sf(gps, coords=c("x", "y"), crs = 4326) 
mapview(points)

points <- points %>% rename(zone_id = "Zone ID", 
                            deploy_date = "Equipment Deployed",
                            retrieval_date = "Equipment Retrieved",
                            study_days_total = "Total Days Studied") %>%
          select(-"GPS Coordinates")

save(points, file="data/processed/0_gps_points.rda")
