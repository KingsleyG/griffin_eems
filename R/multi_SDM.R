###
# Effect on auc of SDM with only Sub-sample of CoralNET data
##

# read in
erad <- readRDS("output/clean_cnet_output/cnet_out.rds")
erad <- data.frame(erad[erad$variable == 
                          "Macroalgae..Large.canopy.forming...Ecklonia", ])     # select kelp only
erad <- erad[erad$Site != "Manly.Point", ]

# define project extent
proj_ext <- extent(c(338206, 342617, 6252119, 6258924))

# read in coastline
est <- readOGR("Spatial/input/shape/", 
               layer = 'SH_poly_clipped')
est <- spTransform(est, CRS("+proj=utm +zone=56 
                            +south +ellps=GRS80 
                            +units=m +no_defs"))
est <- crop(est, proj_ext)

# select only the data we need
erad_vals   <- erad[ , 10]
erad_coords <- erad[ , 7:8]

## Stack spatial predictors
# read in spatial predictors (non temporal)
predrast_dir <- '~Spatial/Processed/raster/SDM_predictors/'       # define directory to shorten code later
pred_names <- list.files(predrast_dir, "*.grd", full.names = TRUE)              # read in file names from pred folder
preds <- stack(pred_names)                                                      # create raster stack from file names
short_names  <- list.files(predrast_dir, "*.grd", full.names = FALSE)           # create shorter names for layers
names(preds) <- short_names                                                     # names the layers based on shorter names
rm(pred_names, short_names)
proj4string(preds) <- proj4string(est)                                          # check CRS is standard for project (UTM)
preds <- dropLayer(preds, 5)

# extract values from raster stack for obs points
predvals <- extract(preds, erad_coords)

# convert percent values to # successful counts
erad_vals <- erad_vals/4

# combine into df with col of 
sdmdata <- data.frame(cbind(erad_vals, predvals))
sdmdata <- na.omit(sdmdata)

## reduce full data by 5% increments (100:5%)
potn <- (10:600)
mod_stats <- data.frame("percent" = NA, "nimages" = NA, "loocvA" = NA)          # setup df for data

for (i in 1:length(potn)){  
  # each interval
  iter <- potn[i]
  
  for(j in 1:5){
  # randomly subset data
  subset_dat <- sdmdata[sample(nrow(sdmdata), iter), ]

  # run glm
  m1 <- glm(cbind(erad_vals, 25-erad_vals) ~ isreef_1m.grd + bathy_1m.grd 
            + latitude_1m.grd + longitude_1m.grd, 
            family = binomial, data = subset_dat)

  # run LOOCV
  loocvi <- cv.glm(subset_dat, m1)$delta
  
  # collate stats into df
  itstat <- c(iter, nrow(subset_dat), loocvi[1])
  mod_stats <- rbind(mod_stats, itstat)
  }
}
mod_stats <- na.omit(mod_stats)

saveRDS(mod_stats, 'output/sdm_cv/multiSDM_Kelp_loocv.rds')

## PLOTTING IN 'R/method_comp_figures.R'
