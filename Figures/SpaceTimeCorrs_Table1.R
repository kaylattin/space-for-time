library(tidyverse)
library(rethinking)
load("Output_TF_Final.RData")

b_space <- summary(stanfit, pars = "b_space")
b_time <- summary(stanfit, pars = "b_time")
a <- summary(stanfit, pars = "a")


x <- b_space$summary[,1]
y <- b_time$summary[,1]

# Creating the plot
plot(x, y, pch = 19, col = "lightblue")

# Regression line
abline(lm(y ~ x), col = "red", lwd = 3)

# Pearson correlation
mtext(paste("Correlation:", round(cor(x, y), 2)))

## set everything up
niterations = 8000
N = 200

bs <- rstan::extract(stanfit, "b_space")
bt <- rstan::extract(stanfit, "b_time")

b_space <- bs$b_space
b_time <- bt$b_time

# prediction fake data
pred_data = seq(from = (min(x) - 0.05), to = (max(x) + 0.05), length.out = 200 )

# initialize
intercept <- vector("list")
slope <- vector("list")
pred_lines <- vector("list")
mu <- matrix(nrow=8000, ncol=200)
corr <- vector("list")
# for every iteration, calculate correlation of b_space (across 33 regions) and b_time (across 33 regions)
# then predict values using the intercept & slope for each iteration
for (i in 1:niterations){
  
  mlm = lm(b_time[i,] ~ b_space[i,])
  
  corr[i] = cor(b_space[i,], b_time[i,])
  
  intercept[i] = mlm$coefficients[[1]] # use mlm or lmodel2?
  slope[i] = mlm$coefficients[[2]]
  
  mu[i,] <- sapply(pred_data,  function(x) mean( unlist(intercept[i]) + unlist(slope[i]) * x ) ) 
  
}

mu.mean <- apply( mu, 2, mean)
ci.mean3 <- apply( mu, 2, PI, prob = 0.95 )


plot(x, y)
lines( pred_data, mu.mean, col = "#2c7bb6", lwd = 1.5)
shade( ci.mean3, pred_data)


# plot these into ggplot?
int_avg <- mean(unlist(intercept))
slope_avg <- mean(unlist(slope))
pi <- PI(unlist(slope), prob = 0.95)
cor_avg <- mean(unlist(corr))
pi_cor <- PI(unlist(corr), prob = 0.95)

slope_avg
pi
cor_avg
pi_cor


load("Output_TFsub_Final.RData")

b_space <- summary(stanfit, pars = "b_space")
b_time <- summary(stanfit, pars = "b_time")
a <- summary(stanfit, pars = "a")


x <- b_space$summary[,1]
y <- b_time$summary[,1]

# Creating the plot
plot(x, y, pch = 19, col = "lightblue")

# Regression line
abline(lm(y ~ x), col = "red", lwd = 3)

# Pearson correlation
mtext(paste("Correlation:", round(cor(x, y), 2)))

## set everything up
niterations = 8000
N = 200

bs <- rstan::extract(stanfit, "b_space")
bt <- rstan::extract(stanfit, "b_time")

b_space <- bs$b_space
b_time <- bt$b_time

# prediction fake data
pred_data = seq(from = (min(x) - 0.05), to = (max(x) + 0.05), length.out = 200 )

# initialize
intercept <- vector("list")
slope <- vector("list")
pred_lines <- vector("list")
mu <- matrix(nrow=8000, ncol=200)
corr <- vector("list")
# for every iteration, calculate correlation of b_space (across 33 regions) and b_time (across 33 regions)
# then predict values using the intercept & slope for each iteration
for (i in 1:niterations){
  
  mlm = lm(b_time[i,] ~ b_space[i,])
  
  corr[i] = cor(b_space[i,], b_time[i,])
  
  intercept[i] = mlm$coefficients[[1]] # use mlm or lmodel2?
  slope[i] = mlm$coefficients[[2]]
  
  mu[i,] <- sapply(pred_data,  function(x) mean( unlist(intercept[i]) + unlist(slope[i]) * x ) ) 
  
}

mu.mean <- apply( mu, 2, mean)
ci.mean3 <- apply( mu, 2, PI, prob = 0.95 )


plot(x, y)
lines( pred_data, mu.mean, col = "#2c7bb6", lwd = 1.5)
shade( ci.mean3, pred_data)


# plot these into ggplot?
int_avg <- mean(unlist(intercept))
slope_avg <- mean(unlist(slope))
pi <- PI(unlist(slope), prob = 0.95)
cor_avg <- mean(unlist(corr))
pi_cor <- PI(unlist(corr), prob = 0.95)

slope_avg
pi
cor_avg
pi_cor



load("Output_TO_Final.RData")

b_space <- summary(stanfit, pars = "b_space")
b_time <- summary(stanfit, pars = "b_time")
a <- summary(stanfit, pars = "a")


x <- b_space$summary[,1]
y <- b_time$summary[,1]

# Creating the plot
plot(x, y, pch = 19, col = "lightblue")

# Regression line
abline(lm(y ~ x), col = "red", lwd = 3)

# Pearson correlation
mtext(paste("Correlation:", round(cor(x, y), 2)))

## set everything up
niterations = 8000
N = 200

bs <- rstan::extract(stanfit, "b_space")
bt <- rstan::extract(stanfit, "b_time")

b_space <- bs$b_space
b_time <- bt$b_time

# prediction fake data
pred_data = seq(from = (min(x) - 0.05), to = (max(x) + 0.05), length.out = 200 )

# initialize
intercept <- vector("list")
slope <- vector("list")
pred_lines <- vector("list")
mu <- matrix(nrow=8000, ncol=200)
corr <- vector("list")
# for every iteration, calculate correlation of b_space (across 33 regions) and b_time (across 33 regions)
# then predict values using the intercept & slope for each iteration
for (i in 1:niterations){
  
  mlm = lm(b_time[i,] ~ b_space[i,])
  
  corr[i] = cor(b_space[i,], b_time[i,])
  
  intercept[i] = mlm$coefficients[[1]] # use mlm or lmodel2?
  slope[i] = mlm$coefficients[[2]]
  
  mu[i,] <- sapply(pred_data,  function(x) mean( unlist(intercept[i]) + unlist(slope[i]) * x ) ) 
  
}

mu.mean <- apply( mu, 2, mean)
ci.mean3 <- apply( mu, 2, PI, prob = 0.95 )


plot(x, y)
lines( pred_data, mu.mean, col = "#2c7bb6", lwd = 1.5)
shade( ci.mean3, pred_data)


# plot these into ggplot?
int_avg <- mean(unlist(intercept))
slope_avg <- mean(unlist(slope))
pi <- PI(unlist(slope), prob = 0.95)
cor_avg <- mean(unlist(corr))
pi_cor <- PI(unlist(corr), prob = 0.95)

slope_avg
pi
cor_avg
pi_cor



load("Output_TOsub_Final.RData")
b_space <- summary(stanfit, pars = "b_space")
b_time <- summary(stanfit, pars = "b_time")
a <- summary(stanfit, pars = "a")


x <- b_space$summary[,1]
y <- b_time$summary[,1]

# Creating the plot
plot(x, y, pch = 19, col = "lightblue")

# Regression line
abline(lm(y ~ x), col = "red", lwd = 3)

# Pearson correlation
mtext(paste("Correlation:", round(cor(x, y), 2)))

## set everything up
niterations = 8000
N = 200

bs <- rstan::extract(stanfit, "b_space")
bt <- rstan::extract(stanfit, "b_time")

b_space <- bs$b_space
b_time <- bt$b_time

# prediction fake data
pred_data = seq(from = (min(x) - 0.05), to = (max(x) + 0.05), length.out = 200 )

# initialize
intercept <- vector("list")
slope <- vector("list")
pred_lines <- vector("list")
mu <- matrix(nrow=8000, ncol=200)
corr <- vector("list")
# for every iteration, calculate correlation of b_space (across 33 regions) and b_time (across 33 regions)
# then predict values using the intercept & slope for each iteration
for (i in 1:niterations){
  
  mlm = lm(b_time[i,] ~ b_space[i,])
  
  corr[i] = cor(b_space[i,], b_time[i,])
  
  intercept[i] = mlm$coefficients[[1]] # use mlm or lmodel2?
  slope[i] = mlm$coefficients[[2]]
  
  mu[i,] <- sapply(pred_data,  function(x) mean( unlist(intercept[i]) + unlist(slope[i]) * x ) ) 
  
}

mu.mean <- apply( mu, 2, mean)
ci.mean3 <- apply( mu, 2, PI, prob = 0.95 )


plot(x, y)
lines( pred_data, mu.mean, col = "#2c7bb6", lwd = 1.5)
shade( ci.mean3, pred_data)


# plot these into ggplot?
int_avg <- mean(unlist(intercept))
slope_avg <- mean(unlist(slope))
pi <- PI(unlist(slope), prob = 0.95)
cor_avg <- mean(unlist(corr))
pi_cor <- PI(unlist(corr), prob = 0.95)

slope_avg
pi
cor_avg
pi_cor

