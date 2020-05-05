#----------------------------------------
#
# Visualizing Spatial Data in R Workshop
# May 6, 2020
#
#----------------------------------------

# Load packages
# Spatial
library(sf)          # Working with vector data 
library(raster)      # Working with raster data
library(rasterVis)   # Visualizing raster data
library(mapview)     # Interactive maps for quick visualization
# Other
library(tidyverse)   # Interacts well with sf objects

#----------------------------------------------
# PART 1 - LOADING AND VISUALIZING SPATIAL DATA

#---------------
#---------------
# Bringing in spatial data and dealing with projections

# From shape files
vegPol <- st_read(dsn = "ShapeFiles/VegLayer", layer = 'VegLayer') # Polygons representing vegetation classes
roadLn <- st_read(dsn = "ShapeFiles/RoadLayer", layer = 'RoadLayer') # Single road running through focal area
# Examine coordinate system
st_crs(vegPol)
st_crs(roadLn)

# From .csv (in the wrong coordinate system)
locPt <- read.csv(file = "LocationData.csv", header = T) # Read in csv file
locPt <- st_as_sf(locPt, coords = c("longitude", "latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Convert to sf object using appropriate crs (https://spatialreference.org/)
locPt <- st_transform(locPt, crs = st_crs(vegPol)) # Project to UTM (using crs from vegPol layer)

# Open multiple raster (geotiff) files as a raster stack
terrStack <- raster::stack(c("Rasters/Slope.tif", "Rasters/Ruggedness.tif"))
st_crs(terrStack) # Check crs

#---------------
#---------------
# Structure of an sf object
vegPol
class(vegPol)

# Plot sf data frames in base plot
par(mar = c(1,1,1,1)) # Reduce margin size
plot(vegPol) # plots each values for each (non-geometry) column separately
plot(vegPol['VegClass']) # plots Area & geometry columns
plot(vegPol["geometry"]) # Plots just the geometry 
plot(st_geometry(vegPol)) # same as above

# Works well with tidyverse functionality...
# Plot just large polygons
vegPol %>% 
  filter(Area > 1000) %>% 
  st_geometry() %>% 
  plot()

# "Sticky" geometry even persists after data manipulations
shrubArea <- vegPol %>% # Get total area of shrub patches
  filter(VegClass == "Shrub") %>% 
  summarise(totalArea = sum(Area))
shrubArea # new data frame with one (multipolygon) feature
plot(st_geometry(shrubArea)) # Still a spatial object - plots only shrubs

# Structure of a single feature
singlePol<-vegPol[329,] # Grab one row from vegPol (= a single polygon feature)
plot(st_geometry(singlePol)) # Plot it
# Get just the coordinates
head(st_coordinates(singlePol))
dim(st_coordinates(singlePol)) 

# Simplify polygons to increase speed of plotting and other spatial operations
singlePol %>% 
  st_simplify(dTolerance = 5) %>% 
  st_geometry() %>% 
  plot(add = T, col = 'steelblue')

# Plotting sf objects in ggplot
ggplot(data = vegPol) +
  geom_sf(aes(fill = VegClass), color = 'black')

#---------------
#---------------
# Viewing rasters
terrStack[[1]]

# Using base plot
plot(terrStack[[1]])
plot(st_geometry(vegPol), add = T)

# Using levelplot from rasterVis
rasterVis::levelplot(terrStack)
levelplot(terrStack[[2]], contour = T, par.settings = viridisTheme())

#---------------
#---------------
# mapview for quick, interactive maps
# Vector
mapview(st_geometry(vegPol), 
        col.regions = c('coral','turquoise')[as.numeric(vegPol$VegClass)], 
        label = round(vegPol$Area))
# Raster
mapview(terrStack[[2]], 
        homebutton = F)


#----------------------------------------------------
# PART 2 - RUNINING AND VISUALIZING SPATIAL OPERATIONS

#---------------
#---------------
# Animal locations within vegetation polygons

# Plot relevant data
plot(st_geometry(vegPol), 
     col = c('grey80','grey40')[as.numeric(vegPol$VegClass)])
plot(st_geometry(locPt), 
     pch = 21, 
     bg = 'magenta',
     cex = 1.5,
     add = T)

# Various spatial subsetting and join functions
?st_within
# Structure of subsetting function output
PtInPol <- st_within(locPt, vegPol, sparse = FALSE)
PtInPol[1:10,1:10] # Creates a locPt x vegPol logical matrix
inPol <- apply(PtInPol, 1, any) # use apply to extract pts in any polygon

# Plot only points within a polygon
locPt %>% 
  filter(apply(st_within(.,vegPol, sparse = F), 1, any)) %>% 
  st_geometry() %>% 
  plot(add = T, pch = 21, bg = 'turquoise', cex = 1.5)

# Get polygon attributes for each point via spatial join
locPt <- st_join(locPt, vegPol, join = st_within)
locPt

# Plot only points within Tree polygons
plot(st_geometry(vegPol), 
     col = c('grey80','grey40')[as.numeric(vegPol$VegClass)])
locPt %>% 
  filter(VegClass == 'Tree') %>% 
  st_geometry() %>% 
  plot(pch = 21, bg = 'turquoise', add = T, cex = 1.5)


#---------------
#---------------
# Polygon distance to nearest line feature
plot(st_geometry(roadLn), lwd = 3, col = 'sienna1', add = T)

# Calculate distance between each polygon and the road
rdist <- st_distance(vegPol, roadLn) # Calculate distance
vegPol$RoadDist<-as.numeric(rdist) # Add to vegPol dataframe

# Visualize gradient in distance from road
ggplot(data = vegPol) +
  geom_sf(aes(fill = RoadDist), color = 'black')
plot(vegPol['RoadDist'])


#---------------
#---------------
# Buffering features
vegPol2 <- filter(vegPol, Area > 1000)
plot(st_geometry(vegPol2), 
     col = c('grey80','grey40')[as.numeric(vegPol2$VegClass)])

# Draw 500 m buffers around polygons
vegPol2 %>% 
  st_buffer(dist = 50) %>% 
  st_geometry() %>% 
  plot(add = T, 
       border = 'skyblue', lwd = 2)


# Union first to buffer the set of features
plot(st_geometry(vegPol2), 
     col = c('grey80','grey40')[as.numeric(vegPol2$VegClass)])
vegPol2 %>% 
  st_union() %>% 
  st_buffer(dist = 50) %>% 
  st_geometry() %>% 
  plot(add = T, 
       border = 'skyblue', lwd = 3)

# Buffer (convex hull of) entire study area
vegPol2 %>% 
  st_union() %>% 
  st_convex_hull() %>% 
  st_buffer(dist = 50) %>%
  st_geometry() %>% 
  plot(add = T, 
       border = 'goldenrod', lwd = 3)

# Each set of buffers is it's own sf data frame, retaining all other columns
st_buffer(vegPol[1:10,], 50)

#---------------
#---------------
# Some raster operations

# Extracting values at point features
locExtract <- raster::extract(terrStack, locPt) # Extract
locPt <- cbind(locPt, locExtract) # Add to data frame
plot(locPt[c("Slope","Ruggedness")], pch = 16, cex = 2) # Visualize

#---------------
# Extracting values across polygon features
# Quick visual
plot(st_geometry(vegPol2[5:10,]))
plot(terrStack[[2]], add = T)
plot(st_geometry(vegPol2[5:10,]), add = T)

# Extract all values with each polygon and take the mean
(vegExtract <- raster::extract(terrStack[[2]], vegPol2[5:10,]))
lapply(vegExtract, mean)

# Do it in one line with tidyverse functions
vegExtract <- raster::extract(terrStack[[2]], vegPol2[5:10,]) %>% 
  map(mean)

#---------------
# Covert polygon to raster

# First get numeric value for vegetation class
# Shrub = 2; Tree = 3
vegPol$ClassNum <- as.numeric(vegPol$VegClass) + 1 

# Create an empty raster with same extent as vegPol
vr <- raster(vegPol, res = 30) # Raster cell size (resolution) = 30x30

# Assign polygon values to raster cells
# Take max polygon value if cell overlaps more than on polygon (fun = max)
vegRast <- rasterize(x = vegPol,
                     y = vr,
                     field = 'ClassNum',
                     fun = max) 

# Open areas (areas not within shrub or tree polygon) reassigned to value 1
vegRast[is.na(vegRast)] <- 1

# Visualize
rasterVis::levelplot(vegRast)
