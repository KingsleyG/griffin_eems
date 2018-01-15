###
# Preparing rasters for SDM
##

library(sp)
library(maptools)
library(raster)
library(rgdal)
library(rgeos)
library(MASS)
library(fields)  # error loading due to new OS == not tested

source('R/functions.R')
# define project extent, bring in coastline
proj_ext <- extent(c(338206, 342616, 6252119, 6258924))
proj_crs <- CRS("+proj=utm +zone=56 +south +ellps=GRS80 +units=m +no_defs")
est <- readOGR("Spatial/input/shape/", layer = "SH_poly_clipped")              # bring in coastline
est <- spTransform(est, proj_crs)
est <- crop(est, proj_ext)                                                     # crop to estuary

# "RREEF" OEH Habitat Mapping Raster
# read in
reef <- readOGR('spatial/input/shape/', "NSW_DPI_Estuarine_Reef", 
                stringsAsFactors = FALSE)                                      # read in shapefiles from OEH Mapping
rast <- Blank.Raster(est, 5)                                                   # blank 5m raster

# 5m raster of binary 'reef' substrate
reef$reefbin <- c(1) # binary to indicate reef present
isreef_rast  <- as.factor(rasterize(x = reef, y = rast, field = 'reefbin'))
isreef_rast[is.na(isreef_rast[])] <- 0
writeRaster(isreef_rast, 'Spatial/SDM_predictors/isreef_5m.grd', 
            overwrite = TRUE)

rm(reef)

## "BATHY" Bathymetry Raster
# read in merge and tidy bathy data
rms <- readOGR("Spatial/input/shape", "sh_soundings_not_spc_mga")
spc <- read.csv("Spatial/input/points/Sydney_Harbour_SPC_5m_grid.csv", header = FALSE)
colnames(spc) <- c("x", "y", "z")
rms_df     <- data.frame(x = rms$coords.x1, y = rms$coords.x2, z = rms$DEPTH)  # convert shp to df
allsound   <- rbind(rms_df, spc)                                               # append the RMS df to the SydPorts df
allsound$z <- turn.neg(allsound$z)                                             # turn positive to negative values

# bring in whole estuary area, to incorporate land in interpolation
whole_est <- readOGR("Spatial/input/shape/", 
                            layer = 'SH_coast_poly_reproj')
whole_est <- spTransform(whole_est, proj_crs)
rast      <- Blank.Raster(whole_est, 5)                                        # blank 5m raster
whole_est$elev <- c(0)                                                         # binary to indicate reef present
outest_rast    <- as.factor(rasterize(x = whole_est, y = rast, field = 'elev'))# rasterise 
outest_rast[is.na(outest_rast[])] <- 1                                         # make area outside of the estuary shp '1'
whole_est <- crop(outest_rast, extent(proj_ext))                               # crop to project area 
whole_est_df   <- data.frame(xyFromCell(whole_est, 1:ncell(whole_est)))        # get xy coords of centers of est cells
whole_est_df$z <- getValues(whole_est)                                         # get values from each cell
whole_est_df   <- whole_est_df[whole_est_df$z == 1, ]                          # subset to only the 'land' cells
allsound       <- rbind(allsound, whole_est_df)                                # append to soundings df

# setup spdf
allsound_spdf <- SpatialPointsDataFrame(coords = allsound[1:2], 
                                        data = allsound[3])                    # spatialise from points
proj4string(allsound_spdf) <- proj4string(rms)                                 #
allsound_spdf <- crop(allsound_spdf, proj_ext)                                 # crop to project area

# clean env
rm(rms, spc, rms_df, allsound, whole_est, outest_rast)

## Rasterise (shape into raster)
rast  <- Blank.Raster(est, 5)                                                  # blank 5m raster with extent of estuary
bathy <- rasterize(allsound_spdf, rast, allsound_spdf$z, mean)                 # rasterise to 5m resolution taking mean of z values

# interpolate to fill gaps in depth raster
bathy_agg  <- aggregate(bathy, 20)                                             # aggregate nearest 20 cells into 1
agg_coords <- data.frame(xyFromCell(bathy_agg, 1:ncell(bathy_agg)))            # get xy coords of centers of aggregated cells
agg_vals   <- getValues(bathy_agg)                                             # get aggregated cell z values
agg_tps    <- Tps(agg_coords, agg_vals)                                        # build thin plate spline regression model using xy coords and z values
bathymetry <- interpolate(bathy, agg_tps)                                      # interpolate out to full extent, from original values
bathy_orig <- mask(bathymetry, bathy, inverse = TRUE)                          # select original cells which didn't need interpolation
bathy_merg <- merge(bathymetry, bathy_orig)                                    # merge original and extrapolated data points
bathy_merg <- mask(bathy_merg, est)                                            # mask out land using estuary coastline shp
#bathy_merg[bathy_merg > 0] <- 0                                                # zero any topo z-values > than 0 (shouldn't happen but still)

# Write to raster grid
bathy <- crop(bathy_merg, est)
extent(bathy) <- proj_ext
extent(bathy) == extent(est)                                              # check project extents so that rasterstack is possible later

writeRaster(bathy, 'Spatial/processed/raster/SDM_predictors/bathy_5m.grd', overwrite = TRUE)

# clean up env
rm(bathy_agg, agg_coords, agg_vals, agg_tps, bathymetry, bathy_orig, bathy_merg, allsound_spdf)

###
# LATITUDE AND LONGITUDE
##
latitude   <- Blank.Raster(est, 5)
lat_xy     <- data.frame(xyFromCell(latitude, 1:length(latitude))) # get coordinates of cell centers
latitude[] <- lat_xy[ , 2] # latitude values == easting/x coordinate
latitude   <- mask(latitude, est)
latitude   <- crop(latitude, extent(est))

longitude   <- Blank.Raster(est, 5)
longitude[] <- lat_xy[ , 1] # longitude values == northing/y value
longitude   <- crop(longitude, extent(est))

writeRaster(latitude, 'Spatial/processed/raster/SDM_predictors/latitude_5m.grd', overwrite = TRUE)
writeRaster(longitude, 'Spatial/processed/raster/SDM_predictors/longitude_5m.grd', overwrite = TRUE)
