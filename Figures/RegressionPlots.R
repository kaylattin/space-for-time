## load up stuff
library(tidyverse)
library(cmdstanr)
library(shinystan)
library(rstan)
library(bayesplot)
library(scales)
library(rethinking)
library(pdftools)
library(rstantools)

library(posterior)
rm(list = ls())
gc()

########## FOREST ##########
d <- read.csv("~/manuscript/FinalDataset_TOsub.csv")

d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

load("Output_TOsub_Final.RData")
draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "retrans_noise"))
y_space <- as.data.frame(draws$y_space)

#draws2 <- as_draws_df(stanfit)
#conv_summ <- summarise_draws(draws2)

pdf("Plots_TOsub_Final.pdf", height = 10)

par(mfrow=c(2,2),
    omi=c(0.3, 0.3, 0.3, 0.3))

for(i in 1:30){
  d_filter <- d %>% filter(Region == i)
  d_filter$Colour= "#2c7bb6"
  d_filter$Shape = 16
  # Set new column values to appropriate colours
  d_filter$Colour[d_filter$space.time == 2] = "#ED432D"
  d_filter$Shape[d_filter$space.time == 2] = 17
  
  d_space <- d_filter %>% filter(space.time == 2)
  d_time <- d_filter %>% filter(space.time == 1)
  
  offs <- mean(log(d$NumForestStops))
  
  x.seq <- seq( from=min(d$Forest.cover), to = max(d$Forest.cover), length.out = 30 )
  
  x.scaled <- (x.seq-mean(d$Forest.cover))/sd(d$Forest.cover)
  mu.space <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 2] + draws$b_space[, i] * x + 
                                                draws$retrans_noise + offs ) ))
  mu.time <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 1] + 
                                                  draws$b_time[, i] * x + 
                                                  draws$retrans_noise + offs )) )
  
  ci.space <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 2] + draws$b_space[, i] * x + 
                                                draws$retrans_noise + offs )  ))
  ci.time <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 1] + 
                                               draws$b_time[, i] * x + 
                                               draws$retrans_noise + offs ) ))
  
  
  plot(d_filter$Forest.cover, d_filter$Richness_avg, 
       col = alpha(d_filter$Colour, 0.6), 
       pch = d_filter$Shape,
       xaxt = "n", yaxt = "n", cex.lab = 1.25, 
       cex.main = 1.5, main = paste("Comparison", i), xlab = "Percent forest cover", ylab = "Species richness")
  

  shade( ci.space, x.scaled, col = alpha("#ED432D", 0.05))
  shade( ci.time, x.scaled, col = alpha("#2c7bb6", 0.05))
  
  lines( x.scaled, mu.space, col = "#ED432D", lwd = 1.5)
  lines( x.scaled, mu.time, col = "#2c7bb6", lwd = 1.5 )
  
}
dev.off()
pdf_convert("Plots_TOsub_Final.pdf",  pages = 1:4)

