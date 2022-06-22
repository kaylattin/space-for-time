library(bayesplot)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)


region <- vector()

for( i in 1:30 ) {
  
  region[i] <- paste("Region", i, sep = " ")
  
}
region <- rev(region)



load("Output_RF_Final.RData")
b_dif_rg <- as.matrix(stanfit, pars = "b_dif_rg")

col <- rep("#253494", 6)
#253494 - richness
#31a354 - abund

### main model - by region
color_scheme_set(col)
  bayesplot_theme_set(theme_classic())
p1 <- mcmc_intervals(b_dif_rg)

p1 <- p1 + vline_0(size = 0.25, color = "darkgray", linetype = 2) + yaxis_text(FALSE) +
  labs(
    x = "Slope difference", 
    y = "Comparison",
    size = 4
  ) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
        axis.text = element_text(size = 12)) + labs(title = "")
p1



load("Output_RO_Final.RData")

b_dif_rg <- as.matrix(stanfit, pars = "b_dif_rg")


col <- rep("#67C5D0", 6)
# #67C5D0 - richness
# #addd8e - abund



color_scheme_set(col)
bayesplot_theme_set(theme_classic())
p2 <- mcmc_intervals(b_dif_rg)

p2 <- p2 + vline_0(size = 0.25, color = "darkgray", linetype = 2) + yaxis_text(FALSE) +
  labs(
    x = "Slope difference", 
    y = "Comparison",
    size = 4
  ) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
        axis.text = element_text(size = 12)) + labs(title = "")



load("Output_RFsub_Final.RData")
b_dif_rg <- as.matrix(stanfit, pars = "b_dif_rg")


col <- rep("#253494", 6)

### main model - by region
color_scheme_set(col)
bayesplot_theme_set(theme_classic())
p3 <- mcmc_intervals(b_dif_rg)

p3 <- p3 + vline_0(size = 0.25, color = "darkgray", linetype = 2) + yaxis_text(FALSE) +
  labs(
    x = "Slope difference", 
    y = "Comparison",
    size = 4
  ) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
        axis.text = element_text(size = 12)) + labs(title = "")



load("Output_ROsub_Final.RData")

b_dif_rg <- as.matrix(stanfit, pars = "b_dif_rg")

col <- rep("#67C5D0", 6)


### main model - by region
color_scheme_set(col)
bayesplot_theme_set(theme_classic())
p4 <- mcmc_intervals(b_dif_rg)

p4 <- p4 + vline_0(size = 0.25, color = "darkgray", linetype = 2) + yaxis_text(FALSE) +
  labs(
    x = "Slope difference", 
    y = "Comparison",
    size = 4
  ) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
        axis.text = element_text(size = 12)) + labs(title = "")
p4

all <- ggarrange(p3, p1, p4, p2,
          ncol = 2, nrow = 2) 

all
ggsave(filename = "RichnessDifferences_Final3.png", device = "png", plot = all,
       width = 30, height = 30, units = "cm")
