#----------------------------------------
#
# Visualizing Spatial Data in R Workshop
# May 6, 2020
#
#----------------------------------------

# Load packages
# Spatial
library(sf)
library(raster)
library(rasterVis)
library(mapview)
# Other
library(tidyverse)



# Plot sf data frames in base plot
plot(vegPol) # plots each values for each (non-geometry) column separately
plot(vegPol['Area']) # plots Area & geometry
plot(vegPol[c('Area',"geometry")]) # same as above
plot(vegPol["geometry"]) # Plots just the geometry 
plot(st_geometry(vegPol)) # same as above

# Plotting sf data frames in ggplot
ggplot(data = vegPol) +
  geom_sf(aes(fill = Area), color = 'black')

# Create a spatial object from scratch
plot(roadCoord)
road_sfg<-st_linestring(roadCoord) # First create the simple feature geometry (sfg class)
road_sfc<-st_sfc(road_sfg, crs = st_crs(vegPol))
road_dat<-data.frame(RoadName = 'road1', RoadLength = round(as.numeric(st_length(road_sfc)),1))
roadLn