library(sf)
library(mapview)

# stadium locations 
{
# RAIDERS NOW IN VEGAS - STADIUM STILL LISTED AS OAKLAND

# stadiums <-
#   st_read("Data/Stadiums/stadiums.json")
}

# city boundaries
{
phoenixBoundary <- 
  st_read("Data/Teams/Arizona Cardinals/Phoenix Data/City_Limit_Dark_Outline.geojson") 

atlantaBoundary <-
  st_union(st_read("Data/Teams/Atlanta Falcons/Atlanta Data/COA_ZipCodes_2020.geojson"))

baltimoreBoundary <- 
  st_read("Data/Teams/Baltimore Ravens/Baltimore Data/Baltimore_City_Line.geojson")

buffaloBoundary <-
  st_read("Data/Teams/Buffalo Bills/Buffalo Data/City Boundary.geojson")

## charlotteBoundary needs to read shapefile

chicagoBoundary <-
  st_read("Data/Teams/Chicago Bears/Chicago Data/Boundaries - City.geojson")
}



# nfl 2019 season data
  
season2019 <- 
    read.csv("Data/Season Data/2019season.csv")
  