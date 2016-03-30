# PROJECT TITLE HERE

# SETUP: README ----------------------------------------------------------------------------------

### This document loads the project's packages and creates any user-defined functions for this specific project.
### Note: some one-time-use "anonymous" functions may be included within other project documents.

# SETUP: LOAD PACKAGES AND PROJECT SETTINGS -------------------------------------------------------

require(scales)         # for ggplot2 label formatting (e.g., 'dollar', 'percent', ect.)
require(gplots)         # for converting colors to HEX strings
require(grDevices)      # for color palettes
require(rgdal)          # for readOGR and others
require(sp)             # for spatial objects
require(rgeos)
require(leaflet)        # for interactive maps (NOT leafletR here)
require(plyr)           # for rounding (the `round_any` function)
require(dplyr)          # for working with data frames
require(ggplot2)        # for plotting
require(tigris)
require(stringr)        # to pad fips codes
require(purrr)
require(magrittr)
require(downloader)
require(tmap)
require(operator.tools) # for the `notin` function
require(tidyr)          # for the `spread` function
require(acs)            # for loading US Census data
require(readr)
# require(rapport)        # for creating a camel case string
# require(htmlwidgets)
# require(classInt)       # for setting breaks in graphs (http://bit.ly/1QexSEP)
# require(spdep)          # for identifying spatial neighbors
require(maptools)       # for combining SpatialPolygonsDataFrames
# require(grid)
# require(gridExtra)
# require(useful)         # for "$150K labeling 
# require(readxl)         # for reading Excel documents
require(stringr)        # for string extraction



options(scipen=999,stringsAsFactors = FALSE)

crs_proj <- CRS("+init=epsg:4326") # This project will use WGS 84 projected coordinate system
crs_geog <- CRS("+init=epsg:2285") # Washington State plane CRS

# SETUP: MY FUNCTIONS --------------------------------------------------------------------

# myLflt

myLflt <- function(data){
        leaflet() %>% 
                addTiles(
                        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
                ) %>% 
                addPolygons(data = data)
}

# mySptlPolyDF
# Quick conversion of objects from class=SpatialPolygons to class=SpatialPolygonsDataFrame
# (this is necessary for saving spatial data in the ESRI Shapefile format)

mySptlPolyDF <- function(shp){
        
        shp_rn <- row.names(shp)
        
        shp_len <- shp@polygons %>% length()
        
        if(length(shp_rn) != shp_len){
                return(message("The `shp` object does not have the same number of row names as the list of polygons"))
        }
        
        else{
                nodata <- rep(NA, times = shp_len) %>% as.data.frame()
                
                rownames(nodata) <- shp_rn
                
                shp %<>% 
                        SpatialPolygonsDataFrame(data = nodata)
                
                return(shp)
        }
        
        
}

mySptlLinesDF <- function(shp){
        
        shp_rn <- row.names(shp)
        
        shp_len <- shp@lines %>% length()
        
        if(length(shp_rn) != shp_len){
                return(message("The `shp` object does not have the same number of row names as the list of polygons"))
        }
        
        else{
                nodata <- rep(NA, times = shp_len) %>% as.data.frame()
                
                rownames(nodata) <- shp_rn
                
                shp %<>% 
                        SpatialLinesDataFrame(data = nodata)
                
                return(shp)
        }
        
}

# wtr_clip
# A function for clipping census geometries (or any other areal data) with waterbody polygons
# Note: the waterbody shapefile should be flattened (`gUnaryUnion`) before being passed to this function

wtr_clip <- function(orig, wtr, path){
        
        new <- 
                gDifference(spgeom1 = orig, spgeom2 = wtr, 
                            byid = TRUE) %>% 
                mySptlPolyDF()
        
        new@data <- 
                gCentroid(new,byid = T) %>% 
                over(., orig) %>% 
                as.data.frame()
        
        new
        
}
