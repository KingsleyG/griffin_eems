###
# Species Distribution Modelling - Probability - KELP!
##

library(sp)
library(data.table)
library(raster)
library(rgdal)
library(rgeos)
library(dismo)
library(grDevices)
library(mgcv)
library(dplyr)
library(reshape2)
library(ggplot2)
source('R/functions.R')

# read in kelp data, clean, prep for spdf
erad <- readRDS("output/clean_cnet_output/cnet_out.rds")
erad <- data.frame(erad[erad$variable == 
                          "Macroalgae..Large.canopy.forming...Ecklonia", ])    # select kelp only
erad <- erad[erad$Site != "Manly.Point", ]                                     # exclude manly point data
pext <- extent(c(338206, 342617, 6252119, 6258924))                            # define project extent
est  <- readOGR("Spatial/input/shape/", 
                layer = 'SH_poly_clipped')                                     # read in coastline
est  <- spTransform(est, CRS("+proj=utm +zone=56 
                             +south +ellps=GRS80 
                             +units=m +no_defs"))                              # transform to correct CRS
est <- crop(est, pext)                                                         # crop to project extent
erad_sites  <- erad[ , 3]; 
erad_vals   <- erad[ , 10]; 
erad_coords <- erad[ , 7:8]                                                    # select only the data we need
erad_vals   <- erad_vals/4                                                     # convert percent to n# successes

# read in spatial predictors
predrast_dir <- 'Spatial/Processed/raster/SDM_predictors/'                     # dir to shorten code later
pred_names   <- list.files(predrast_dir, "*5m.grd", full.names = TRUE)         # file names from pred folder
preds        <- stack(pred_names)                                              # raster stack from list of files
short_names  <- list.files(predrast_dir, "*5m.grd", full.names = FALSE)        # shorter layer names
names(preds) <- short_names                                                    # name layers based on shorter names
rm(pred_names, short_names, predrast_dir)
proj4string(preds) <- proj4string(est)                                         # CRS standard for project (UTM)

# arrange model training data
predval <- extract(preds, erad_coords)                                         # extract predictor values at sample locations
sdmdata <- data.table(erad_sites, erad_vals, predval)                          # combine into df and define col types
sdmdata <- na.omit(sdmdata)
sdmdata$isreef_5m.grd <- as.factor(sdmdata$isreef_5m.grd)
sdmdata$erad_sites    <- as.factor(sdmdata$erad_sites)
sdmdata <- as.data.frame(sdmdata)
rm(predval, erad_sites, erad_vals)

# correlation between predictors
pdf('figures/baseR/pairspanel.pdf', width = 7, height = 7)                     # print pairspanel to pdf for supplementary figs
pairs.panels(sdmdata[ , 2:6])                                                  # plot paired correlations between predictors
dev.off()

# Modelling
m_erad <- glm(cbind(erad_vals, 25-erad_vals) ~ isreef_5m.grd + bathy_5m.grd 
              + latitude_5m.grd + longitude_5m.grd, 
              family = binomial, data = sdmdata)
sink("R/SDM_kelp_5m_summary.txt")                                              # 'sink' to text file for reproduction in supp's
summary(m_erad)                                                                # all covariates are significant
drop1(m_erad)                                                                  # all covariates should be retained
sink()

pdf("figures/baseR/SDM_kelp_5m_diagnostics.pdf")                               # save diagnostics plots for reproduction in supp's
par(mfrow = c(2,2))
plot(m_erad)
dev.off()


# Cross-validation

# kfold AUC cross-validation of full model
k     <- 10
group <- kfold(sdmdata, 10)

e <- list()
for (i in 1:k) {
  train <- sdmdata[group != i, ]
  test  <- sdmdata[group == i, ]
  tm_erad <- glm(cbind(erad_vals, 25-erad_vals) ~ isreef_5m.grd + bathy_5m.grd 
                 + latitude_5m.grd + longitude_5m.grd, 
                 family = binomial, data = train)
  e[[i]] <- evaluate(p=test[test$erad_vals > 0, ], 
                     a=test[test$erad_vals == 0, ], tm_erad)
}

auc <- sapply(e, function(x){slot(x, 'auc')})
saveRDS(auc, 'output/sdm_cv/SDM_Kelp5m_auc.rds')                               # save AUC for quoting in publication
mean(auc)                                                                      # check base AUC
rm(k, group, e, train, test, tm_erad)

# leave one out cross-validation against all data
 require(boot)
 loocv <- cv.glm(sdmdata, m_erad)$delta 
 saveRDS(loocv, 'output/sdm_cv/SDM_Kelp5m_loocv.rds')


# Model Predictions

p    <- predict(preds, m_erad, fun=pred.and.se, index=1:2)                     # predict across estuary, saving se.
p_se <- p$layer.2
p    <- p$layer.1

# mask predictions to only viable reef
reef                 <- preds[[2]]
reef[reef == 0]      <- NA
p_reef  <- mask(p, reef)
se_reef <- mask(p_se, reef)

# save predictions
writeRaster(p_reef, "Spatial/output/SDM_predictions/ecklp_reefonly_5m", 
            format = "GTiff", overwrite = TRUE)
writeRaster(se_reef, "Spatial/output/SDM_predictions/ecklse_reefonly_5m", 
            format = "GTiff", overwrite = TRUE)
