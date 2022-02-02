## load up stuff
library(tidyverse)
library(cmdstanr)
library(shinystan)
library(rstan)
library(bayesplot)
library(scales)
library(rethinking)
library(pdftools)

rm(list = ls())
gc()

########## FOREST ##########

d <- read.csv("~/manuscript/FinalDataset_RO.csv")
d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

load("Output_RO_Poisson.RData")
draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "observer", "first"))

pdf("Plots_RF.pdf", height = 10)

par(mfrow=c(2,2),
    omi=c(0.3, 0.3, 0.3, 0.3))

for(i in 1:31){
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
  s <- unique(d_space$NumForestStops)
  s2 <- unique(d_time$NumForestStops)
  
  x.seq <- seq( from=min(d_filter$Cover_std), to = max(d_filter$Cover_std), length.out = 200 )
  
  
  mu.space <- sapply(x.seq,  function(x) mean( exp(mean(draws$a[, i, 2]) + mean(draws$observer[, n]) + mean(draws$first[,1:2]) + mean(draws$b_space[, i]) * x ) ) ) 
  mu.time <- sapply(x.seq,  function(x) mean( exp(draws$a[, i, 1] + draws$observer[, n2] + mean(draws$first[,1:2]) + draws$b_time[, i] * x ) ) ) 
  
  #ci.space <- sapply(x.seq,  function(x) PI( exp(draws$a[, i, 2]) + exp(mean(draws$observer[, n])) + exp(mean(draws$first[,1:2])) + exp(draws$b_space[, i]) * x ) ) 
  #ci.time <- sapply(x.seq,  function(x) PI( exp(draws$a[, i, 1]) + exp(draws$observer[, n2]) + exp(mean(draws$first[,1:2]))  + exp(draws$b_time[, i]) * x ) ) 
  
  
  plot(d_filter$Cover_std, d_filter$Richness, col = alpha(d_filter$Colour, 0.6), pch = d_filter$Shape,
       xaxt = "n", yaxt = "n", cex.lab = 1.25, cex.main = 1.5, 
       main = paste("Region", i), xlab = "Percent forest cover", ylab = "Species richness")
  
  axis(1, at = c(min(d_filter$Cover_std), median(d_filter$Cover_std), max(d_filter$Cover_std)), 
       labels = c(round(min(d_filter$Forest.cover),2), round(median(d_filter$Forest.cover),2), round(max(d_filter$Forest.cover),2)),
       cex.axis = 1.25)
  axis(2, cex.axis = 1.25)

  #shade( ci.space, x.seq, col = alpha("#ED432D", 0.05))
  #shade( ci.time, x.seq, col = alpha("#2c7bb6", 0.05))
  
  lines( x.seq, mu.time, col = "#2c7bb6", lwd = 1.5)
  lines( x.seq, mu.space, col = "#ED432D", lwd = 1.5 )
  
}
dev.off()
pdf_convert("Plots_RF.pdf",  pages = 1:4)
