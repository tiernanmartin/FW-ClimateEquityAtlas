# PROJECT TITLE HERE

# SETUP: README ----------------------------------------------------------------------------------

### This document creates the data objects that will be analyzed.


# SETUP: FUNCTIONS AND OPTIONS ----------------------------------------------------------------------------------

source("./1_r_scripts/1_setup_functions.R") # load the project functions

# DATA: SEATTLE TRACTS ----------------------------------------------------------------------------

seattle <- {
        if(!exists("./2_inputs/seattle.shp")){
                make_seattle <- function(){
                        
                        seattle <- tigris::places(state = "WA") %>% 
                                tigris::filter_place(place = "Seattle") %>% 
                                spTransform(CRSobj = crs_proj)
                }
                
                seattle <- make_seattle()
                
                rm(make_seattle)
                
                seattle
                }
}

water <- {
        if(!file.exists("./2_inputs/water.shp")){
                
                make_water <- function(){
                        if(!file.exists("./2_inputs/NHDMajor.gdb")){  # check if the file already exists, if not then download it
                                url <- "ftp://www.ecy.wa.gov/gis_a/inlandWaters/NHD/NHDmajor.gdb.zip" # save the URL for the waterbodies data
                                
                                temp <- tempfile() # create a temporary file to hold the compressed download
                                
                                download(url, dest = temp, mode="wb") # download the file
                                
                                unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
                                
                                dateDownloaded <- date()
                        }
                        
                        path_gdb <- "./2_inputs/NHDMajor.gdb/" # path to the geodatabase folder
                        
                        water <- 
                                readOGR(dsn = path_gdb,      # create a waterbodies shape
                                        layer = "NHD_MajorWaterbodies") %>%
                                gBuffer(byid=TRUE, width=0) %>% # clean up self-intersecting polygons
                                spTransform(CRSobj = crs_proj) # transform the projection to match the project projection
                        
                        intersect <- gIntersects(seattle,water,byid = TRUE) %>% as.vector()
                        
                        writeOGR(obj = water,
                                 dsn = "./2_inputs/",
                                 layer = "water",
                                 driver = "ESRI Shapefile")
                        
                        water <- water[intersect,] %>% 
                                spTransform(CRSobj = crs_proj)
                }
                water <- make_water()
                rm(make_water)
                water
                
        } else 
                readOGR(dsn = "./2_inputs/",layer = "water") %>% spTransform(CRSobj = crs_proj)
}

seattle_noWater <- {
        if(!file.exists("./2_inputs/seattle_noWater.shp")){
                
                make_seattle_noWater <- function(){
                        wtr <- gUnaryUnion(water)
                        wtr_clip(seattle,wtr)
                        
                }
                seattle_noWater <- make_seattle_noWater()
                rm(make_seattle_noWater)
                seattle_noWater
                
        }
}

seattle_tr <- {
        
        if(!file.exists("./2_inputs/seattle_tr.shp")){
                make_seattle_tr <- function(){

                        trList <- read_csv(file = "./2_inputs/DEC_10_SF1_H1_with_ann.csv") %>%
                                mutate(GEOID6 = str_sub(GEO.id2, start= -6))

                        tr <- tigris::tracts(state = "WA", county = "King") %>%
                                spTransform(CRSobj = crs_proj)
                        tr@data %<>%
                                mutate(GEOID6 = str_sub(GEOID, start= -6))
                        tr %<>% subset(GEOID6 %in% trList$GEOID6)

                        sea_noWtr <- gUnaryUnion(seattle_noWater)

                        tr <- wtr_clip(tr,wtr)

                        writeOGR(obj = tr,
                                 dsn = "./2_inputs/",
                                 layer = "seattle_tr",
                                 driver = "ESRI Shapefile",
                                 overwrite_layer = TRUE)

                        tr
                }

                seattle_tr <- make_seattle_tr()

                rm(make_seattle_tr)
                seattle_tr
        } else
                readOGR(dsn = "./2_inputs/",
                       layer = "seattle_tr") %>% 
                spTransform(CRSobj = crs_proj)
        
        
}

# DATA: ACS --------------------------------------------------------------------------------------- 