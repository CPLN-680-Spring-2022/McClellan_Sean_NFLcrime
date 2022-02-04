library(tidyverse)
library(sf)
library(RSocrata)
library(viridis)
library(spatstat)
library(raster)
library(spdep)
library(FNN)
library(grid)
library(gridExtra)
library(knitr)
library(kableExtra)
library(tidycensus)
library(crsuggest)
library(mapview)
library(lubridate)
library(httr)
library(gridExtra)
library(rgeos)

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")


# stadium locations - CAROLINA MISPELLED IN STADIUM FILE

stadiums <-
  st_read("Data/Stadiums/stadiums.json") %>%
  st_transform("EPSG:4326")

linc <- 
  stadiums %>%
  filter(Team == "Philadelphia Eagles")

lincBuffer <-
  st_buffer(linc, 1000) %>%
  st_transform(st_crs(stadiums))

mt <- 
  stadiums %>%
  filter(Team == "Baltimore Ravens")

mtBuffer <-
  st_buffer(mt, 1000) %>%
  st_transform(st_crs(stadiums))

ralphwilson <-
  stadiums %>%
  filter(Team == "Buffalo Bills")

ralphwilsonBuffer <-
  st_buffer(ralphwilson, 1000) %>%
  st_transform(st_crs(stadiums))

BofA <-
  stadiums %>%
  filter(Team == "Carolia Panthers")

BofABuffer <-
  st_buffer(BofA, 1000) %>%
  st_transform(st_crs(stadiums))

soldier <-
  stadiums %>%
  filter(Team == "Chicago Bears")

soldierBuffer <-
  st_buffer(soldier, 1000) %>%
  st_transform(st_crs(stadiums))

# city boundaries

phoenixBoundary <- 
  st_read("Data/Teams/Arizona Cardinals/City_Limit_Dark_Outline.geojson") %>%
    st_transform("EPSG:4326")

atlantaBoundary <-
  st_union(st_read("Data/Teams/Atlanta Falcons/COA_ZipCodes_2020.geojson")) %>%
  st_transform("EPSG:4326")

baltimoreBoundary <- 
  st_read("Data/Teams/Baltimore Ravens/Baltimore_City_Line.geojson") %>%
  st_transform("EPSG:4326")

buffaloBoundary <-
  st_read("Data/Teams/Buffalo Bills/City Boundary.geojson") %>%
  st_transform("EPSG:4326")

charlotteBoundary <-
  st_union(st_read("Data/Teams/Carolina Panthers/City_Council_Districts.shp")) %>%
  st_transform("EPSG:4326")

chicagoBoundary <-
  st_read("Data/Teams/Chicago Bears/Boundaries - City.geojson") %>%
  st_transform("EPSG:4326")

philadelphiaBoundary <- 
  st_read("Data/Teams/Philadelphia Eagles/City_Limits.geojson") %>%
  st_transform("EPSG:4326")

philadelphiaLiquor <- 
  st_read("https://services.arcgis.com/fLeGjb7u4uXqeF9q/ArcGIS/rest/services/PLCB_LIQUOR_LICENSES/FeatureServer")


# nfl 2019 season data
  
season2019 <- 
    read.csv("Data/Season Data/2019season.csv")

# import crime data

baltimoreCrime <-
  st_read("Data/Teams/Baltimore Ravens/Part1_Crime_data.geojson") %>%
  st_transform("EPSG:4326") %>%
  mutate(date = ymd_hms(CrimeDateTime),
         year = year(date)) %>%
  filter(year == 2019, Latitude > 0)

buffaloCrime <-
  st_read("Data/Teams/Buffalo Bills/Crime Incidents.geojson") %>%
  st_transform("EPSG:4326") %>%
  mutate(date = ymd_hms(incident_datetime),
         year = year(date)) %>%
  filter(year == 2019)

charlotteCrime <- 
  st_read("Data/Teams/Carolina Panthers/CMPD_Incidents.geojson") %>%
  st_transform("EPSG:4326") %>%
  filter(YEAR == 2019)

chicagoCrime <- 
  read_csv("Data/Teams/Chicago Bears/Crimes_-_2019.csv") %>%
  na.omit() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform('EPSG:4326')
  
philadelphiaCrime <-
  st_read("Data/Teams/Philadelphia Eagles/incidents_part1_part2.shp") %>%
  st_transform("EPSG:4326") %>%
  filter(point_y > 30)

# Plot Crime Density

ggplot() + 
  geom_sf(data = philadelphiaBoundary, color = "black", fill = "gray20", size=1.3) +
  geom_sf(data = philadelphiaBoundary, color = "black", fill = "gray60", size=0.1) +
  stat_density2d(data = data.frame(st_coordinates(philadelphiaCrime)), 
                 aes(X, Y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 70, geom = 'polygon') +
  scale_fill_viridis() +
  scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
  labs(title = "Density of Phl Crime") +
  mapTheme(title_size = 14) + theme(legend.position = "none")

ggplot() + 
  geom_sf(data = baltimoreBoundary, color = "black", fill = "gray20", size=1.3) +
  geom_sf(data = baltimoreBoundary, color = "black", fill = "gray60", size=0.1) +
  stat_density2d(data = data.frame(st_coordinates(baltimoreCrime)), 
                 aes(X, Y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 70, geom = 'polygon') +
  scale_fill_viridis() +
  scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
  geom_sf(data = mt, size = 3) + 
  labs(title = "Density of Bal Crime") +
  mapTheme(title_size = 14) + theme(legend.position = "none")

ggplot() + 
  geom_sf(data = buffaloBoundary, color = "black", fill = "gray20", size=1.3) +
  geom_sf(data = buffaloBoundary, color = "black", fill = "gray60", size=0.1) +
  stat_density2d(data = data.frame(st_coordinates(buffaloCrime)), 
                 aes(X, Y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 70, geom = 'polygon') +
  scale_fill_viridis() +
  scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
  geom_sf(data = ralphwilson, size = 3) + 
  labs(title = "Density of Buf Crime") +
  mapTheme(title_size = 14) + theme(legend.position = "none")

ggplot() + 
  geom_sf(data = charlotteBoundary, color = "black", fill = "gray20", size=1.3) +
  geom_sf(data = charlotteBoundary, color = "black", fill = "gray60", size=0.1) +
  stat_density2d(data = data.frame(st_coordinates(charlotteCrime)), 
                 aes(X, Y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 70, geom = 'polygon') +
  scale_fill_viridis() +
  scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
  geom_sf(data = BofA, size = 3) + 
  labs(title = "Density of Car Crime") +
  mapTheme(title_size = 14) + theme(legend.position = "none")

ggplot() + 
  geom_sf(data = chicagoBoundary, color = "black", fill = "gray20", size=1.3) +
  geom_sf(data = chicagoBoundary, color = "black", fill = "gray60", size=0.1) +
  stat_density2d(data = data.frame(st_coordinates(chicagoCrime)), 
                 aes(X, Y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 40, geom = 'polygon') +
  scale_fill_viridis() +
  scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
  geom_sf(data = soldier, size = 3) + 
  labs(title = "Density of Chi Crime") +
  mapTheme(title_size = 14) + theme(legend.position = "none")

mapview(stadiums)

ggplot() +
  geom_sf(data = philadelphiaBoundary, color = "black", fill = "gray20", size=1.3) +
  geom_sf(data = philadelphiaBoundary, color = "black", fill = "gray60", size=0.1) +
  geom_sf(data = lincBuffer, color = "green", fill = "lightgreen", alpha = 0.5) +
  stat_density2d(data = data.frame(st_coordinates(philadelphiaCrime)), 
                 aes(X, Y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 70, geom = 'polygon') +
  scale_fill_viridis() +
  scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
  geom_sf(data = linc, size = 1.5, color = "darkgreen") +
  labs(title = "Density of Phl Crime") +
  mapTheme(title_size = 14) + theme(legend.position = "none")


# hex grid

make_grid <- function(x, cell_diameter, cell_area, clip = FALSE) {
  if (missing(cell_diameter)) {
    if (missing(cell_area)) {
      stop("Must provide cell_diameter or cell_area")
    } else {
      cell_diameter <- sqrt(2 * cell_area / sqrt(3))
    }
  }
  ext <- as(extent(x) + cell_diameter, "SpatialPolygons")
  projection(ext) <- projection(x)
  # generate array of hexagon centers
  g <- spsample(ext, type = "hexagonal", cellsize = cell_diameter, 
                offset = c(0.5, 0.5))
  # convert center points to hexagons
  g <- HexPoints2SpatialPolygons(g, dx = cell_diameter)
  # clip to boundary of study area
  if (clip) {
    g <- gIntersection(g, x, byid = TRUE)
  } else {
    g <- g[x, ]
  }
  # clean up feature IDs
  row.names(g) <- as.character(1:length(g))
  return(g)
}

 

fishnet <- 
  st_make_grid(philadelphiaBoundary,
               cellsize = 500, 
               square = FALSE) %>%     
  st_sf() %>%
  mutate(uniqueID = rownames(.))

crime_net <- 
  dplyr::select(philadelphiaCrime) %>% 
  mutate(countCrimes = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countCrimes = replace_na(countCrimes, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), 
                       size=nrow(fishnet), replace = TRUE))
