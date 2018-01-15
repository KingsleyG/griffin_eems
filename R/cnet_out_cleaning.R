###
# Data:    Sydney Catlin Seaview Survey (CORALNET)
# Task:    Wrangling CoralNet Output Data
# author:  Kingsley Griffin
##

source("R/functions.R")
library(data.table)
library(reshape2)
library(dplyr)
library(rgdal)
library(sp)

# read data into DT
cnet_out <- list.files("data/coralnet_output/latest", 
                       "*.csv", full.names = TRUE)
cnet_out <- read.csv(cnet_out, stringsAsFactors = FALSE)
cnet_out <- data.table(cnet_out)

# remove duplicate images
cnet_out <- unique(cnet_out)

# remove irrelevant columns and any unused labels (col == 0)
cnet_out <- cnet_out[ , (6:12) := NULL]
cnet_out <- cnet_out[ , (9:11) := NULL]
cnet_out <- subset(cnet_out, select = c(!apply(cnet_out == 0, 2, all)))
setkey(cnet_out, Original.File.Name)

# fix format of LatLong coordinates
cnet_out$Latitude  <- CoordFix(cnet_out$Latitude)
cnet_out$Longitude <- CoordFix(cnet_out$Longitude)

# convert Lat Long coordinates to DD
cnet_out$Latitude  <- sapply(cnet_out$Latitude, LatLongToDD)
cnet_out$Longitude <- sapply(cnet_out$Longitude, LatLongToDD)
cnet_out$Latitude  <- turn.neg(cnet_out$Latitude)

# remove rows with NA Latitude/Longitude values (only 2 col with NA's)
cnet_out <- na.omit(cnet_out)

# convert DD Lat Long to UTM
coords <- cbind(cnet_out$Longitude, (cnet_out$Latitude))
coords <- SpatialPoints(coords, proj4string=CRS("+proj=longlat +ellps=WGS84
                                                +datum=WGS84"))
coords <- spTransform(coords, CRS("+proj=utm +zone=56 +south +ellps=GRS80
                                  +units=m +no_defs"))
coords <- as.data.frame(coords)
coords <- data.table(coords)
cnet_out$northing <- coords[[2]]
cnet_out$easting  <- coords[[1]]

# introduce small amount of jitter to differentiate points
set.seed(1) # fixed random outcomes, regardless of reruns
rand <- rnorm(length(cnet_out$Original.File.Name), mean = 0, sd = 0.1)
cnet_out$easting <- cnet_out$easting + rand
set.seed(2)
rand2 <- rnorm(length(cnet_out$Original.File.Name), mean = 0, sd = 0.1)
cnet_out$northing <- cnet_out$northing + rand2

# select only rows with < 15 % cover of sand or gravel
cnet_out <- cnet_out[cnet_out$Substrate..Unconsolidated..Sand.Mud...2mm. < 15 &
                       cnet_out$Substrate..Unconsolidated...pebble.gravel < 15]

# save RDS and .csv for use elsewhere
saveRDS(cnet_out, "output/clean_cnet_output/cnet_out_wide.rds")
write.csv(cnet_out, file = "output/clean_cnet_output/cnet_out_wide.csv")

# melt transform
cnet_out <- melt(cnet_out, id = c("Region", "date_taken", "Site", 
                                  "Original.File.Name", "Quadrat", 
                                  "annotation_status", "easting", 
                                  "northing"))

# reorder data table to suit content
cnet_out <- cnet_out[order(Original.File.Name, Region, date_taken, Site, 
                           Quadrat, annotation_status, easting, northing, 
                           variable, value)]

cnet_out$variable <- as.character(cnet_out$variable)

write.csv(cnet_out, file = "output/clean_cnet_output/cnet_out.csv")

# save cleaned df's to RDS and csv
saveRDS(cnet_out, "output/clean_cnet_output/cnet_out.rds")


# clean up env
rm(list = ls())

