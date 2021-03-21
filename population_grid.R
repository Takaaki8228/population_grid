
# ------------------------------------
# Setup
library(ggplot2)
library(dplyr)
library(sf)
# library(ggthemes)
library(viridis)
library(extrafont)

setwd("~/Documents/GitHub/Drawing-Population-Maps-in-R")

shiga <- read_sf("1km_mesh_suikei_2018_shape_25/1km_mesh_2018_25.shp")
kyoto <- read_sf("1km_mesh_suikei_2018_shape_26/1km_mesh_2018_26.shp")
osaka <- read_sf("1km_mesh_suikei_2018_shape_27/1km_mesh_2018_27.shp")
hyogo <- read_sf("1km_mesh_suikei_2018_shape_28/1km_mesh_2018_28.shp")
nara <- read_sf("1km_mesh_suikei_2018_shape_29/1km_mesh_2018_29.shp")
wakayama <- read_sf("1km_mesh_suikei_2018_shape_30/1km_mesh_2018_30.shp")


# ------------------------------------
# Draw Maps
kansai <- ggplot() + 
  geom_sf(data = shiga, color = "red") +
  geom_sf(data = kyoto, color = "green") +
  geom_sf(data = osaka, color = "blue") +
  geom_sf(data = hyogo, color = "cyan") +
  geom_sf(data = nara, color = "magenta") +
  geom_sf(data = wakayama, color = "yellow") +
  theme_minimal() + theme(panel.grid = element_blank()) + 
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))
kansai
ggsave(kansai, filename = "kansai.png", width = 6, height = 8)

# ------------------------------------
# Calculate centroid of polygon using sf::st_centroid

shiga$centroids <- st_transform(shiga, 6674) %>% 
  st_centroid() %>% 
  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  st_geometry() 

kyoto$centroids <- st_transform(kyoto, 6674) %>% 
  st_centroid() %>% 
  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  st_geometry() 

osaka$centroids <- st_transform(osaka, 6674) %>% 
  st_centroid() %>% 
  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  st_geometry() 

hyogo$centroids <- st_transform(hyogo, 6674) %>% 
  st_centroid() %>% 
  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  st_geometry() 

nara$centroids <- st_transform(nara, 6674) %>% 
  st_centroid() %>% 
  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  st_geometry() 

wakayama$centroids <- st_transform(wakayama, 6674) %>% 
  st_centroid() %>% 
  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  st_geometry() 


# ------------------------------------
# Convert to data frame
shiga <- as.data.frame(shiga, xy = T)
kyoto <- as.data.frame(kyoto, xy = T)
osaka <- as.data.frame(osaka, xy = T)
hyogo <- as.data.frame(hyogo, xy = T)
nara <- as.data.frame(nara, xy = T)
wakayama <- as.data.frame(wakayama, xy = T)


# ------------------------------------
# Extract and split lat long coordinates from the point
# refer: https://stackoverflow.com/questions/38637070/extract-and-split-lat-long-coordinates-from-wkt-point-data-in-r
shiga$long <- as.numeric(gsub(".*?([-]*[0-9]+[.][0-9]+).*", "\\1", shiga$centroids))
shiga$lat <- as.numeric(gsub(".* ([-]*[0-9]+[.][0-9]+).*", "\\1", shiga$centroids))

kyoto$long <- as.numeric(gsub(".*?([-]*[0-9]+[.][0-9]+).*", "\\1", kyoto$centroids))
kyoto$lat <- as.numeric(gsub(".* ([-]*[0-9]+[.][0-9]+).*", "\\1", kyoto$centroids))

osaka$long <- as.numeric(gsub(".*?([-]*[0-9]+[.][0-9]+).*", "\\1", osaka$centroids))
osaka$lat <- as.numeric(gsub(".* ([-]*[0-9]+[.][0-9]+).*", "\\1", osaka$centroids))

hyogo$long <- as.numeric(gsub(".*?([-]*[0-9]+[.][0-9]+).*", "\\1", hyogo$centroids))
hyogo$lat <- as.numeric(gsub(".* ([-]*[0-9]+[.][0-9]+).*", "\\1", hyogo$centroids))

nara$long <- as.numeric(gsub(".*?([-]*[0-9]+[.][0-9]+).*", "\\1", nara$centroids))
nara$lat <- as.numeric(gsub(".* ([-]*[0-9]+[.][0-9]+).*", "\\1", nara$centroids))

wakayama$long <- as.numeric(gsub(".*?([-]*[0-9]+[.][0-9]+).*", "\\1", wakayama$centroids))
wakayama$lat <- as.numeric(gsub(".* ([-]*[0-9]+[.][0-9]+).*", "\\1", wakayama$centroids))


# ------------------------------------
# Clean the data
shiga <- shiga %>%
  dplyr::select(long, lat, PTN_2015) %>%
  dplyr::select(long, lat, PTN_2015, everything()) %>%
  dplyr::rename(Population = PTN_2015)

kyoto <- kyoto %>%
  dplyr::select(long, lat, PTN_2015) %>%
  dplyr::select(long, lat, PTN_2015, everything()) %>%
  dplyr::rename(Population = PTN_2015)

osaka <- osaka %>%
  dplyr::select(long, lat, PTN_2015) %>%
  dplyr::select(long, lat, PTN_2015, everything()) %>%
  dplyr::rename(Population = PTN_2015)

hyogo <- hyogo %>%
  dplyr::select(long, lat, PTN_2015) %>%
  dplyr::select(long, lat, PTN_2015, everything()) %>%
  dplyr::rename(Population = PTN_2015)

nara <- nara %>%
  dplyr::select(long, lat, PTN_2015) %>%
  dplyr::select(long, lat, PTN_2015, everything()) %>%
  dplyr::rename(Population = PTN_2015)

wakayama <- wakayama %>%
  dplyr::select(long, lat, PTN_2015) %>%
  dplyr::select(long, lat, PTN_2015, everything()) %>%
  dplyr::rename(Population = PTN_2015)


# Append the prefectures' data
kansai_pop <- dplyr::bind_rows(
  shiga, kyoto, osaka, hyogo, nara, wakayama
)



# ------------------------------------
# Draw a map (1)
pop_map1 <- ggplot() + 
  geom_point(data= kansai_pop, 
             aes(x = long, y = lat, color = Population),
             alpha = 0.5) + 
  scale_color_gradient(low = "#edd6d4", high = "#7A0018") + 
  labs(x = NULL, 
       y = NULL, 
       title = "Kansai's Regional Demographics", 
       subtitle = "Population in Kansai Municipalities, 2015", 
       caption = "Source: National Spatial Planning and Regional Policy Bureau") +
  theme_void() +
  theme(
    plot.title = element_text(vjust = -2, hjust = 0.1),
    plot.subtitle = element_text(vjust = -2, hjust = 0.1),
    plot.caption = element_text(vjust = 3, hjust = 0.8),
    text = element_text(family = "Optima"),
    plot.background = element_rect(fill = "#f5f5f2"), 
    legend.position = c(0.13, 0.18))

pop_map1
ggsave(pop_map1, filename = "pop_map1.png", width = 6, height = 8)



# Draw a map (2)
pop_map2 <- ggplot() + 
  geom_point(data= kansai_pop, 
             aes(x = long, y = lat,
                 size = Population, color = Population),
             alpha = 0.3) + 
  scale_color_gradient(low = "#edd6d4", high = "#7A0018") + 
  # Show a legend for a colour
  guides(size = FALSE) + 
  scale_size_area(max_size = 15) + 
  labs(x = NULL, 
       y = NULL, 
       title = "Kansai's Regional Demographics", 
       subtitle = "Population in Kansai Municipalities, 2015", 
       caption = "Source: National Spatial Planning and Regional Policy Bureau") +
  theme_void() +
  theme(
    plot.title = element_text(vjust = -2, hjust = 0.1),
    plot.subtitle = element_text(vjust = -2, hjust = 0.1),
    plot.caption = element_text(vjust = 3, hjust = 0.8),
    text = element_text(family = "Optima"),
    plot.background = element_rect(fill = "#f5f5f2"), 
    legend.position = c(0.13, 0.18))

pop_map2
ggsave(pop_map2, filename = "pop_map2.png", width = 6, height = 8)
