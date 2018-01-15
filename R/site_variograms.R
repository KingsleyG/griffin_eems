###
# Data:    Sydney Catlin Seaview Survey (CORALNET)
# Task:    Assessing spatial structuring in observations with variograms
# author:  Kingsley Griffin
##

library(ncf)
# source('R/transects.R')

# Correlogram to show spatial structure amongst sites
cor.all <- data.frame()                                                         # clean df for populating

for (i in 1:length(unique(erad_df$Site))){
  site    <- unique(erad_df$Site)[i]
  site_sp <- erad_df[erad_df$Site == site, ]
  sp.cor  <- correlog(site_sp$easting, site_sp$northing, site_sp$value, 
                      increment = 5, resamp = 100, quiet = TRUE)
  cor.df  <- data.frame("location" = rep(site, length(sp.cor$n)),
                          "lag" = 1:length(sp.cor$n), sp.cor[2], 
                        sp.cor[3], sp.cor[5], sp.cor[4])                        # convert to data frame for ggplot
  cor.all <- rbind(cor.all, cor.df)                                             # bind overall site data by row
}

erad_df$null <- sample(0:25, length(erad_spdf), replace = T) * 4                # create null data
cor.nul <- data.frame()                                                         # clean df for populating

for (i in 1:length(unique(erad_df$Site))){
  site    <- unique(erad_df$Site)[i]
  site_sp <- erad_df[erad_df$Site == site, ]
  sp.cor  <- correlog(site_sp$easting, site_sp$northing, site_sp$null, 
                      increment = 5, resamp = 0, quiet = TRUE)
  cor.df  <- data.frame("location" = rep(site, length(sp.cor$n)),
                        "lag" = 1:length(sp.cor$n), sp.cor[2], 
                        sp.cor[3], sp.cor[4])                                   # convert to data frame for ggplot
  cor.nul <- rbind(cor.nul, cor.df)                                             # bind overall site data by row
}


## Plotting
siteord <- c("Chowder.Bay", "Balgowlah", "Manly.Outside",
             "Shark.Bay", "North.Head", "N.Bondi",
             "South.Head", "Middle.Head")                                       # order as per 'R/method_comp_figures.R'
cor.all <- within(cor.all, location <- factor(location, levels = siteord))      # rearrange df to suit
cor.nul <- within(cor.nul, location <- factor(location, levels = siteord))      # rearrange df to suit
cor.nul$location <- as.factor(cor.nul$location)
cor.all$location <- as.factor(cor.all$location)
cor.all <- na.omit(cor.all)
cor.nul <- na.omit(cor.nul)
