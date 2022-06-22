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

d <- read.csv("~/space-for-time/FinalDataset_TFsub.csv")

d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

load("Output_TFsub_Final.RData")
draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "retrans_noise", "observer", "first"))
y_space <- as.data.frame(draws$y_space)

#draws2 <- as_draws_df(stanfit)
#conv_summ <- summarise_draws(draws2)

pdf("Plots_TFsub_Final.pdf", height = 10)

par(mfrow=c(4,2),
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
  
  n <- unique(d_space$Obs_ID)
  n2 <- unique(d_time$Obs_ID)
  
  offs <- mean(log(d$NumStops))
  
  x.seq <- seq( from=min(d$Forest.cover), to = max(d$Forest.cover), length.out = 30 )
  
  x.scaled <- (x.seq-mean(d$Forest.cover))/sd(d$Forest.cover)
  mu.space <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 2] + draws$b_space[, i] * x + 
                                                       mean(draws$observer[, n]) + mean(draws$first[,1:2]) + 
                                                       draws$retrans_noise + offs ) ))
  mu.time <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                      mean(draws$observer[, n2]) + mean(draws$first[,1:2]) +
                                                      draws$retrans_noise + offs )) )
  
  ci.space <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 2] + draws$b_space[, i] * x + 
                                                     mean(draws$observer[, n]) + mean(draws$first[,1:2]) +
                                                     draws$retrans_noise + offs )) )
  ci.time <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                    mean(draws$observer[, n2]) + mean(draws$first[,1:2]) +
                                                    draws$retrans_noise + offs )) )
  
  
  plot(d_filter$Forest.cover, d_filter$TA, 
       col = alpha(d_filter$Colour, 0.6), 
       pch = d_filter$Shape,
       yaxt = "n", cex.lab = 1.25, 
       cex.main = 1.5, main = paste("Comparison", i), xlab = "Percent forest cover", ylab = "Species richness")
  
  
  axis(2, cex.axis = 1.25)
  
  shade( ci.space, x.scaled, col = alpha("#ED432D", 0.05))
  shade( ci.time, x.scaled, col = alpha("#2c7bb6", 0.05))
  
  lines( x.scaled, mu.space, col = "#ED432D", lwd = 1.5)
  lines( x.scaled, mu.time, col = "#2c7bb6", lwd = 1.5 )
  
}
dev.off()
pdf_convert("~/space-for-time/Plots_TFsub_Final.pdf",  pages = 1:4)

