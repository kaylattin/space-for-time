library(bayesplot)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)


region <- vector()

for( i in 1:31 ) {
  
  region[i] <- paste("Region", i, sep = " ")
  
}
region <- rev(region)



load("Output_RF_Poisson.RData")
b_dif_rg <- as.matrix(stanfit, pars = "b_dif_rg")

col <- rep("#253494", 6)
#253494 - richness
#31a354 - abund

### main model - by region
color_scheme_set(col)
  bayesplot_theme_set(theme_classic())
p1 <- mcmc_intervals(b_dif_rg)

p1 <- p1 + vline_0(size = 0.25, color = "darkgray", linetype = 2) + yaxis_text(FALSE)
p1



load("Output_RO_Poisson.RData")

b_dif_rg <- as.matrix(stanfit, pars = "b_dif_rg")


col <- rep("#67C5D0", 6)
# #67C5D0 - richness
# #addd8e - abund



color_scheme_set(col)
bayesplot_theme_set(theme_classic())
p2 <- mcmc_intervals(b_dif_rg)

p2 <- p2 + vline_0(size = 0.25, color = "darkgray", linetype = 2) + yaxis_text(FALSE)



load("Output_RFsub_Poisson.RData")
b_dif_rg <- as.matrix(stanfit, pars = "b_dif_rg")


col <- rep("#253494", 6)

### main model - by region
color_scheme_set(col)
bayesplot_theme_set(theme_classic())
p3 <- mcmc_intervals(b_dif_rg)

p3 <- p3 + vline_0(size = 0.25, color = "darkgray", linetype = 2) + yaxis_text(FALSE)



load("Output_ROsub_Poisson.RData")

b_dif_rg <- as.matrix(stanfit, pars = "b_dif_rg")

col <- rep("#67C5D0", 6)


### main model - by region
color_scheme_set(col)
bayesplot_theme_set(theme_classic())
p4 <- mcmc_intervals(b_dif_rg)

p4 <- p4 + vline_0(size = 0.25, color = "darkgray", linetype = 2) + yaxis_text(FALSE)
p4

all <- ggarrange(p3, p1, p4, p2,
          ncol = 2, nrow = 2) 

all
ggsave(filename = "RichnessDifferences.png", device = "png", plot = all,
       width = 30, height = 30, units = "cm")
