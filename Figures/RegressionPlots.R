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
d <- read.csv("FinalDataset_RF.csv")


d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

load("Output_RF_Poisson_TEST.RData")
draws <- rstan::extract(stanfit, pars = c("a", "y_space", "y_time", "b_space", "b_time",
                                          "retrans_noise"))
y_space <- as.data.frame(draws$y_space)

draws2 <- as_draws_df(stanfit)
conv_summ <- summarise_draws(draws2)


list <- vector("list")
list2 <- vector ("list")

for(i in 1:30){
  for(n in 1:30){
    
    list[n] <- mean(as.numeric(unlist(y_space[paste0(i, ".", n, sep = "")])))
    list2[[i]] <- list
    
  }
  print(paste0("Progress: ", round((i/31)*100, 2), "% finished."))
}

y_time <- as.data.frame(draws$y_time)

list <- vector("list")
list3 <- vector ("list")

for(i in 1:30){
  for(n in 1:30){
    
    list[n] <- mean(as.numeric(unlist(y_time[paste0(i, ".", n, sep = "")])))
    list3[[i]] <- list
    
  }
  print(paste0("Progress: ", round((i/31)*100, 2), "% finished."))
}


pdf("Plots_RF_TEST_simple.pdf", height = 10)

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
  mu.space <- sapply(x.scaled,  function(x) {exp(mean(draws$a[, i, 2]) + mean(draws$b_space[, i]) * x + 
                                                mean(draws$retrans_noise) + offs ) }) 
  mu.time <- sapply(x.scaled,  function(x) {exp(mean(draws$a[, i, 2]) + 
                                                  mean(draws$b_time[, i]) * x + 
                                                  mean(draws$retrans_noise ) + offs )}) 
  
  #ci.space <- sapply(x.seq,  function(x) PI( exp(draws$a[, i, 2]) + exp(mean(draws$observer[, n])) + exp(mean(draws$first[,1:2])) + exp(draws$b_space[, i]) * x ) ) 
  #ci.time <- sapply(x.seq,  function(x) PI( exp(draws$a[, i, 1]) + exp(draws$observer[, n2]) + exp(mean(draws$first[,1:2]))  + exp(draws$b_time[, i]) * x ) ) 
  
  
  plot(d_filter$Forest.cover, d_filter$Richness_avg, 
       col = alpha(d_filter$Colour, 0.6), 
       pch = d_filter$Shape,
       xaxt = "n", yaxt = "n", cex.lab = 1.25, 
       cex.main = 1.5, main = paste("Comparison", i), xlab = "Percent forest cover", ylab = "Species richness")
  

  #shade( ci.space, x.seq, col = alpha("#ED432D", 0.05))
  #shade( ci.time, x.seq, col = alpha("#2c7bb6", 0.05))
  
  # lines( x.seq, list2[[i]], col = "#2c7bb6", lwd = 1.5)
  # lines( x.seq, list3[[i]], col = "#ED432D", lwd = 1.5 )
  # 
  
  lines( x.seq, mu.space, col = "#2c7bb6", lwd = 1.5)
  lines( x.seq, mu.time, col = "#ED432D", lwd = 1.5 )
  
}
dev.off()
pdf_convert("Plots_RF_TEST_simple.pdf",  pages = 1:4)

