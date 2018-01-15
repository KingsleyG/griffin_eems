###
# Figures for Chapter 1 - Comparing contemporary methods with traditional diver based surveys of algal cover on temperate reefs.
##
library(ggplot2)

# Get data and sort df to suit figures
source('R/transects.R')
siteord <- c("Chowder.Bay", "Balgowlah", "Manly.Outside",
             "Shark.Bay", "North.Head", "N.Bondi",
             "South.Head", "Middle.Head")                                       # set site order for figures
erad_df <- within(erad_df, Site <- factor(Site, levels = siteord))              # rearrange df to suit
tsects  <- summarise(group_by(erad_df, Site), 
                     "meanpcobs" = round(mean(value), 0),
                     "sd_pcobs"  = round(sd(value), 0),
                     "se_pcobs"  = round(sd(value)/sqrt(n()), 2), 
                     "n_pcobs"   = n())                                         # summarise transects at each site

## Figure 1 - map of survey locations, made in QGIS.
### Figure 2 - tvalues from increasing #n transects compared w. whole surveys
allstats <- within(allstats, Site <- factor(Site, levels = siteord))            # rearrange df to suit
site_txt <- data.frame(ntrans  = 28, tvalue = max(abs(allstats$tvalue)) - 1, 
                       sitelab = c("A", "B", "C", "D", "E", "F", "G", "H"),
                       Site    = siteord)
mean_txt <- data.frame(ntrans  = 20, tvalue = max(abs(allstats$tvalue)) - 3, 
                       meanlab = c(tsects$meanpcobs),
                       selab   = c(tsects$se_pcobs),
                       nlab = c(tsects$n_pcobs),
                       Site = siteord)                                          # fix mean cover annotations
p <- ggplot(data = allstats, aes(ntrans, abs(tvalue)))
p + geom_smooth(formula = abs(y) ~ x, colour = "black",
                alpha = 3/4, lwd = 0.25) +
  geom_point(alpha = 1/2, size = 0.25) + 
  geom_hline(yintercept = 2, lty=3) +
  xlab("replicate transects (n)") + 
  ylab("t - value (absolute)") + 
  facet_wrap( ~ Site) +
  geom_text(data = site_txt,
            aes(label = paste(sitelab))) +
  geom_text(data = mean_txt, size = 2,
            aes(label = paste("µ=", meanlab, "%",
                              "+/-", selab, 
                              "n=", nlab))) +
  guides(size = FALSE) + 
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        strip.background = element_blank(),
        strip.text = element_blank())

# ggsave("figures/ggplot/facet_tval_tsects.png",
#        width = 15, height = 12, units = "cm", dpi = 600)

## Figure 3 - correlograms faceted by site
source('R/site_variograms.R')                                                   # will take a little while ~10min
site_txt <- data.frame(mean.of.class = 350, correlation = 0.65, 
                       sitelab = c("A", "B", "C", "D", "E", "F", "G", "H"),
                       location = siteord)
ggplot(data = cor.all, aes(x = mean.of.class, y = correlation, group = location)) + 
  geom_smooth(data = cor.nul, colour = "black", alpha = 3/4, lwd = 0.25,
              aes(x = mean.of.class, y = correlation)) +
  geom_line(alpha = 1/2) +
  geom_point(data = cor.all, aes(alpha = 1/p), size = 0.25) + 
  geom_segment(data = cor.all, alpha = 1/4, size = 0.25, lty = 3,
               aes(x = x.intercept, xend = x.intercept, y=-0.5, yend=0.5)) +  
  geom_text(data = site_txt, aes(label = paste(sitelab))) +
  xlim(0, 350) +
  facet_wrap( ~ location, nrow = 4, ncol = 2) +
  ylab('Correlation (Moran\'s I)') + 
  xlab('distance (m)') +
  guides(size = FALSE, alpha = FALSE) +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        strip.background = element_blank(),
        strip.text = element_blank())

# ggsave("figures/ggplot/correl_sites.png",
#        width = 15, height = 14, units = "cm", dpi = 300)

## Figure 4 - SDM predicted Kelp Distribution and SE.
source('R/SDM_Kelp_5m.R')                                                          # this will take a while..
#then make final figure in QGIS - basically just:
plot(p_reef)
plot(reefse)

## Figure 5 - LOOCV accuracy against volume of training data
#source('R/multi_SDM.R')                                                         # will take about 30min
mod_stats  <- readRDS("output/sdm_cv/multiSDM_Kelp_loocv.rds")
full_loocv <- readRDS("output/sdm_cv/SDM_Kelp_loocv.rds")
ggplot(data = mod_stats, aes(nimages, loocvA)) + 
  geom_hline(aes(yintercept = full_loocv[1]), linetype = "dashed") +
  geom_point(alpha = 1/4) + 
  geom_smooth(colour = "lightgrey", size = 0.5, alpha = 1) +
  theme_bw() + 
  theme(text = element_text(size = 10)) +
  guides(alpha = FALSE) +  
  xlab("n# quadrats") + 
  ylab("Estimated prediction error (n=5)") +
  xlim(0, 450) + ylim(0, 0.2)

ggsave("figures/ggplot/multiSDM_loocv.png",
       width = 15, height = 10, units = "cm", dpi = 300)

