###
# Data:    Sydney Catlin Seaview Survey (CORALNET)
# Task:    Subsetting survey data into 50m 'transect' sections
# author:  Kingsley Griffin
##

source('R/functions.R')
library(ggplot2)
library(raster)
library(dplyr)
library(data.table)
library(reshape2)
library(sp)
library(rgeos)

# Read in Ecklonia Data
erad <- readRDS("output/clean_cnet_output/cnet_out.rds")
erad <- erad[erad$variable == "Macroalgae..Large.canopy.forming...Ecklonia"]    # this is the only 
erad <- erad[erad$Site != "Manly.Point", ]                                      # remove Manly Point and Shelley Beach sites (insufficient data for transects)
erad <- erad[erad$Site != "Shelley.Beach", ]
  
# Spatialise data
erad_coords <- data.frame(east = erad$easting, 
                           north = erad$northing)
erad_spdf   <- SpatialPointsDataFrame(coords = erad_coords, data = erad, 
                                      proj4string=CRS("+proj=utm +zone=56 
                                                      +south +ellps=GRS80 
                                                      +units=m +no_defs"))
rm(erad, erad_coords)

## Record distance in m along each transect
erad_df <- data.frame()                                                         # prep dfs
erad_spdf$dist.to.prev <- 0
erad_spdf$trans.length <- 0

for (i in 1:length(unique(erad_spdf$Site))){                                    # for each unique site
  site_sam <- erad_spdf[erad_spdf$Site == ((unique(erad_spdf$Site)[i])), ]      # subset to iteration of site
  for (i in 1:length(site_sam)){                                                # for every row of site
    if (i != 1){
      iter <- site_sam[i, ]
      prev <- site_sam[i-1, ]
      site_sam$dist.to.prev[i] <- gDistance(iter, prev)                         # calc dist between i and prev point
      site_sam$trans.length[i] <- (site_sam$dist.to.prev[i] + 
                                   site_sam$trans.length[i-1])                  # total distance = distance to previous point + previous transect length
    } else {
      site_sam$dist.to.prev[i] <- 0
      site_sam$trans.length[i] <- 0
    }
    }
    site_sam_df <- as.data.frame(site_sam)
    erad_df <- rbind(erad_df, site_sam_df)
}
erad_df$transect.no <- round(erad_df$trans.length/50, 0) + 1                    # add transect number (total length/50m) +1 to avoid first ts being 0
rm(i, site_sam, iter, prev)

# T-test increasing n of transects against whole dataset
allstats   <- data.frame("Site" = NA, "ntrans" = NA, "tvalue" = NA, "df" = NA, 
                         "p.val" = NA, "meanpc" = NA, "sepc" = NA)              # prep df for stats
ttest_erad <- erad_df[erad_df$Quadrat == "1", ]
                                                                                # iteratively,
for(i in 1:5){                                                                  # five times
for (l in 1:length(unique(ttest_erad$Site))){                                   # for each unique site
  site_sam <- ttest_erad[ttest_erad$Site == ((unique(ttest_erad$Site)[l])), ]   # subset to iteration of site
  poss_ts  <- unique(site_sam$transect.no)                                      # all possible transect #'s at the given site
  trans_df <- data.frame()
  for (j in 1:(length(poss_ts))){                                               # check each possible transect has at least 10 quadrats
    trans <- site_sam[site_sam$transect.no == poss_ts[j], ]
    if (nrow(trans) < 10){
    } else {
trans_df <- rbind(trans_df, trans)
  }
  }
  site_ts  <- unique(trans_df$transect.no)                                      # all possible transect #'s wth >10 quads at the given site
  sitestat <- data.frame("Site" = NA, "ntrans" = NA, "tvalue" = NA,
                         "df" = NA, "p.val" = NA, "meanpc" = NA, "sepc" = NA)   # set up df
 for (k in 1:(length(site_ts))){                                                # run iterations for 1 to max no. of possible transects
    ranpts <- sample(site_ts, k, replace = FALSE)                               # randomly select i transects from possible transects
    ts_pts <- site_sam[site_sam$transect.no %in% ranpts, ]                      # select points where transect.no == randomly selected transect #s
    ts_pts <- ts_pts[ts_pts$Quadrat == "1", ]                                   # only quadrat 1 from each image (50 x 1m transects)
    ttest  <- t.test(ts_pts$value, site_sam$value)                              # run independent paired t-test between i x 1 x 50m and full site kelp cover
    sitestat[k, ] <- summarise(ts_pts, unique(Site), k, takenum(ttest[1]),
                               takenum(ttest[2]), takenum(ttest[3]), 
                               mean(value), se(sd(ts_pts$value), n())
                               )                                                # return stats on transect data
  }
allstats <- rbind(allstats, sitestat)
}
}
allstats <- na.omit(allstats)
rm(i, j, k, l)

# FOR PLOTTING - see 'R/method_comp_figures.R'
