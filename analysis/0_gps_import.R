library(sf); library(tidyverse); library(readxl); library(stringr); library(mapview); library(ggthemes)

gps <-  read_excel("data/raw/final_gps_points.xlsx")

gps <- gps %>% rename(zone_id = "Zone",
                      gps_coord = "Kristy GPS Coordinates",
                      fox_detected = "Fox Detected"
                      ) %>% mutate(fox_detected = replace_na(fox_detected, "No"))

temp <- as.data.frame(str_split(gps$gps_coord, " "))

gps$y <- as.character(temp[1,]) %>% str_replace(",", "") %>% str_pad(width = 10, side = "right", pad = 0)
gps$x <- as.character(temp[2,]) %>% str_replace(",", "") %>% str_pad(width = 10, side = "right", pad = 0)

points <- st_as_sf(gps, coords=c("x", "y"), crs = 4326) 
mapview(points)

points <- points %>% select(-gps_coord) %>% mutate(x= gps$x, y = gps$y)
          
save(points, file="data/processed/0_gps_points.rda")

theme <- theme_clean() + theme(plot.background = element_rect(fill="white", color = "white"),
                               panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
                               axis.line = element_line(colour = "white"),
                               legend.position = "bottom")

border <- read_sf("data/raw/uk_border_simplified.shp")

ggplot(points, aes(colour = fox_detected, fill = fox_detected)) +
  geom_polygon(data=border, colour = "black") +
  geom_sf(size=2) +
  theme + labs(y="", x="")

p <- ggplot(points) +
      geom_sf(data = border, fill= "white") +
      geom_sf(size = 2, aes(colour = fox_detected)) + 
      scale_colour_manual(values = c("navy", "lightseagreen")) + 
      theme + labs(y = "", x = "", colour = "Visited by a fox")
  
ggsave(p, file = "data/points_on_map.svg", units = "cm", dpi = "retina", width = 20, height = 30)
ggsave(p, file = "data/points_on_map.png", units = "cm", dpi = "retina", width =25, height = 60)
