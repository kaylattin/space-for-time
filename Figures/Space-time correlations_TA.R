# Load in libraries
library(tidyverse)
library(ggpubr)
library(rstan)
library(ggrepel)
library(gridExtra)
library(rethinking)
library(shinystan)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ## RICHNESS - FOREST BIRDS - ALL ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("Output_TF_Poisson.RData")

# Extract parameters for ggplot
b_space <- summary(stanfit, pars = "b_space") # spatial slopes
b_time <- summary(stanfit, pars = "b_time") # temporal slopes
a <- summary(stanfit, pars = "a") # intercept

# Extract the slopes into a response and predictor variable (space = x, time = y)
x <- b_space$summary[,1]
y <- b_time$summary[,1]

bs <- rstan::extract(stanfit, "b_space") # extract spatial slopes
bt <- rstan::extract(stanfit, "b_time") # extract temporal slopes

b_space <- bs$b_space # assign to a referenceable object
b_time <- bt$b_time

### R PLOTTING (based on Statistical Rethinking by Richard McElreath) ### 
# Set up fake data
pred_data = seq(from = (min(x) - 0.05), to = (max(x) + 0.05), length.out = 200 )

# Initialize
niterations = 8000
N = 200
intercept <- vector("list")
slope <- vector("list")
pred_lines <- vector("list")
mu <- matrix(nrow=8000, ncol=200)

# The code below cycles through every MCMC iteration (total of 8000) and calculates correlation of b_space (across 33 regions) and b_time (across 33 regions)
# Then, it predicts plottable relationship using the intercept & slope for each iteration
for (i in 1:niterations){
  
  mlm = lm(b_time[i,] ~ b_space[i,]) # linear regression between time and space
  
  intercept[i] = mlm$coefficients[[1]] # extract intercept
  slope[i] = mlm$coefficients[[2]] # extract slope between space-time
  
  mu[i,] <- sapply(pred_data,  function(x) mean( unlist(intercept[i]) + unlist(slope[i]) * x ) ) # derive plottable values for mean relationship between time and space
  
}

mu.mean <- apply( mu, 2, mean ) # mean of distribution of y for each value of x
ci.mean <- apply( mu, 2, PI, prob = 0.95 ) # calculate confidence interval

# Plot in R
plot(x, y)
lines( pred_data, mu.mean, col = "#2c7bb6", lwd = 1.5)
shade( ci.mean, pred_data)


#### GGPLOT ####
## Now move onto ggplot and just feed the intercept & slope in; compare to above (which is the correct way of doing it)
r <- data.frame(x, y)
r$Region <- seq(1:31)

d <- read.csv("~/manuscript/FinalDataset_TF.csv")

# Set up a column that contains info on how many spatial sites are associated with each temporal site
spatial <- d %>% filter(space.time == 2)
spatialSites <- spatial %>% group_by(ref) %>% summarize(nsites = n_distinct(RouteNumber)) %>% select(ref, nsites)
spatialSites$Region <- as.integer(as.factor(spatialSites$ref))

# Merge the column into the main dataframe
r <- merge(r, spatialSites, by = "Region")

# Take the mean of all intercepts and slopes calculated by the 8000 regressions between time & space
int_avg <- mean(unlist(intercept))
slope_avg <- mean(unlist(slope))
conf <- data.frame(pred_data, mu.mean)

## GGplot object
p1 <- ggplot(r, mapping = aes(x, y)) +
  geom_point(
    colour = "#31a354",
    alpha = 0.8,
    aes(size = nsites)
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    alpha = 0.5,
    size = 0.5
  ) +
  geom_abline(
    intercept = int_avg,
    slope = slope_avg,
    colour = "#31a354",
    size = 1
  ) +
  labs(
    x = "Spatial slope", 
    y = "Temporal slope",
    size = 4
  ) +
  theme_classic()


p1 <- p1 + theme(plot.margin = unit(c(1,1,1,1),"cm"),
                 plot.title = element_text(size = 20, face = "bold"),
                 axis.title = element_text(size = 14), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
                 axis.text = element_text(size = 12)) + labs(title = "") +
  labs(title = expression(paste("II. ", italic(N[spatial]),phantom() > phantom(),italic(N[temporal]))), size = "Spatial sample size") +
  geom_ribbon(data = conf, mapping = aes(x = pred_data, y = mu.mean,
                                         xmin = min(pred_data), xmax = max(pred_data),
                                         ymin = ci.mean[1,],
                                         ymax = ci.mean[2,]),
              fill = "#31a354",
              alpha = 0.1)
p1


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ## RICHNESS - OPEN HABITATS BIRDS - ALL ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("Output_TO_Poisson.RData")
# Extract parameters for ggplot
b_space <- summary(stanfit, pars = "b_space") # spatial slopes
b_time <- summary(stanfit, pars = "b_time") # temporal slopes
a <- summary(stanfit, pars = "a") # intercept

# Extract the slopes into a response and predictor variable (space = x, time = y)
x <- b_space$summary[,1]
y <- b_time$summary[,1]

bs <- rstan::extract(stanfit, "b_space") # extract spatial slopes
bt <- rstan::extract(stanfit, "b_time") # extract temporal slopes

b_space <- bs$b_space # assign to a referenceable object
b_time <- bt$b_time


### R PLOTTING (based on Statistical Rethinking by Richard McElreath) ### 
# Set up fake data
pred_data = seq(from = (min(x) - 0.05), to = (max(x) + 0.05), length.out = 200 )

# Initialize
niterations = 8000
N = 200
intercept <- vector("list")
slope <- vector("list")
pred_lines <- vector("list")
mu <- matrix(nrow=8000, ncol=200)

# The code below cycles through every MCMC iteration (total of 8000) and calculates correlation of b_space (across 33 regions) and b_time (across 33 regions)
# Then, it predicts plottable relationship using the intercept & slope for each iteration
for (i in 1:niterations){
  
  mlm = lm(b_time[i,] ~ b_space[i,]) # linear regression between time and space
  
  intercept[i] = mlm$coefficients[[1]] # extract intercept
  slope[i] = mlm$coefficients[[2]] # extract slope between space-time
  
  mu[i,] <- sapply(pred_data,  function(x) mean( unlist(intercept[i]) + unlist(slope[i]) * x ) ) # derive plottable values for mean relationship between time and space
  
}

mu.mean2 <- apply( mu, 2, mean ) # mean of distribution of y for each value of x
ci.mean2 <- apply( mu, 2, PI, prob = 0.95 ) # calculate confidence interval

# Plot in R
plot(x, y)
lines( pred_data, mu.mean2, col = "#2c7bb6", lwd = 1.5)
shade( ci.mean2, pred_data)


#### GGPLOT ####
## Now move onto ggplot and just feed the intercept & slope in; compare to above (which is the correct way of doing it)
r <- data.frame(x, y)
r$Region <- seq(1:31)

d <- read.csv("~/manuscript/FinalDataset_TO.csv")

# Set up a column that contains info on how many spatial sites are associated with each temporal site
spatial <- d %>% filter(space.time == 2)
spatialSites <- spatial %>% group_by(ref) %>% summarize(nsites = n_distinct(RouteNumber)) %>% select(ref, nsites)
spatialSites$Region <- as.integer(as.factor(spatialSites$ref))

# Merge the column into the main dataframe
r <- merge(r, spatialSites, by = "Region")

# Take the mean of all intercepts and slopes calculated by the 8000 regressions between time & space
int_avg <- mean(unlist(intercept))
slope_avg <- mean(unlist(slope))
conf2 <- data.frame(pred_data, mu.mean2)

# Extract parameters for ggplot
b_space <- summary(stanfit, pars = "b_space") # spatial slopes
b_time <- summary(stanfit, pars = "b_time") # temporal slopes
a <- summary(stanfit, pars = "a") # intercept

# Extract the slopes into a response and predictor variable (space = x, time = y)
x <- b_space$summary[,1]
y <- b_time$summary[,1]

## GGplot object
p2 <- ggplot(r, mapping = aes(x, y)) +
  geom_point(
    colour = "#addd8e",
    alpha = 0.8,
    aes(size = nsites)
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    alpha = 0.5,
    size = 0.5
  ) +
  geom_abline(
    intercept = int_avg,
    slope = slope_avg,
    colour = "#addd8e",
    size = 1
  ) +
  labs(
    x = "Spatial slope", 
    y = "Temporal slope",
    size = 4
  ) +
  theme_classic()


p2 <- p2 + theme(plot.margin = unit(c(0,1,1,1),"cm"),
                 axis.title = element_text(size = 14), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
                 axis.text = element_text(size = 12)) + labs(title = "") + labs(title = "", size = "Spatial sample size") +
  geom_ribbon(data = conf2, mapping = aes(x = pred_data, y = mu.mean2,
                                          xmin = min(pred_data), xmax = max(pred_data),
                                          ymin = ci.mean2[1,],
                                          ymax = ci.mean2[2,]),
              fill = "#addd8e",
              alpha = 0.1)
p2



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ## RICHNESS - FOREST BIRDS - SUBSET ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("Output_TFsub_Poisson.RData")

# Extract parameters for ggplot
b_space <- summary(stanfit, pars = "b_space") # spatial slopes
b_time <- summary(stanfit, pars = "b_time") # temporal slopes
a <- summary(stanfit, pars = "a") # intercept

# Extract the slopes into a response and predictor variable (space = x, time = y)
x <- b_space$summary[,1]
y <- b_time$summary[,1]

bs <- rstan::extract(stanfit, "b_space") # extract spatial slopes
bt <- rstan::extract(stanfit, "b_time") # extract temporal slopes

b_space <- bs$b_space # assign to a referenceable object
b_time <- bt$b_time


# Set up fake data
pred_data = seq(from = (min(x) - 0.05), to = (max(x) + 0.05), length.out = 200 )

# Initialize
niterations = 8000
N = 200
intercept <- vector("list")
slope <- vector("list")
pred_lines <- vector("list")
mu <- matrix(nrow=8000, ncol=200)

# The code below cycles through every MCMC iteration (total of 8000) and calculates correlation of b_space (across 33 regions) and b_time (across 33 regions)
# Then, it predicts plottable relationship using the intercept & slope for each iteration
for (i in 1:niterations){
  
  mlm = lm(b_time[i,] ~ b_space[i,]) # linear regression between time and space
  
  intercept[i] = mlm$coefficients[[1]] # extract intercept
  slope[i] = mlm$coefficients[[2]] # extract slope between space-time
  
  mu[i,] <- sapply(pred_data,  function(x) mean( unlist(intercept[i]) + unlist(slope[i]) * x ) ) # derive plottable values for mean relationship between time and space
  
}

mu.mean3 <- apply( mu, 2, mean ) # mean of distribution of y for each value of x
ci.mean3 <- apply( mu, 2, PI, prob = 0.95 ) # calculate confidence interval

# Plot in R
plot(x, y)
lines( pred_data, mu.mean3, col = "#2c7bb6", lwd = 1.5)
shade( ci.mean3, pred_data)


#### GGPLOT ####
## Now move onto ggplot and just feed the intercept & slope in; compare to above (which is the correct way of doing it)
r <- data.frame(x, y)
r$Region <- seq(1:31)

d <- read.csv("~/manuscript/FinalDataset_TF.csv")

# Set up a column that contains info on how many spatial sites are associated with each temporal site
spatial <- d %>% filter(space.time == 2)
spatialSites <- spatial %>% group_by(ref) %>% summarize(nsites = n_distinct(RouteNumber)) %>% select(ref, nsites)
spatialSites$Region <- as.integer(as.factor(spatialSites$ref))

# Merge the column into the main dataframe
r <- merge(r, spatialSites, by = "Region")

# Take the mean of all intercepts and slopes calculated by the 8000 regressions between time & space
int_avg <- mean(unlist(intercept))
slope_avg <- mean(unlist(slope))
conf3 <- data.frame(pred_data, mu.mean3)

# Extract parameters for ggplot
b_space <- summary(stanfit, pars = "b_space") # spatial slopes
b_time <- summary(stanfit, pars = "b_time") # temporal slopes
a <- summary(stanfit, pars = "a") # intercept

# Extract the slopes into a response and predictor variable (space = x, time = y)
x <- b_space$summary[,1]
y <- b_time$summary[,1]


## all regions
p3 <- ggplot(r, mapping = aes(x,y)) + 
  geom_point(
    colour = "#31a354",
    alpha = 0.8,
    size = 3,
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    alpha = 0.5,
    size = 0.5
  ) +
  geom_abline(
    intercept = int_avg,
    slope = slope_avg,
    colour = "#31a354",
    size = 1
  ) +
  labs(
    x = "Spatial slope", 
    y = "Temporal slope",
    size = 4
  ) +
  theme_classic()


p3 <- p3 + theme(plot.margin = unit(c(1,1,1,1),"cm"),
                 plot.title = element_text(size = 20, face = "bold"),
                 axis.title = element_text(size = 14), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
                 axis.text = element_text(size = 12)) + labs(title = "") +
  labs(title = expression(paste("I. ", italic(N[spatial]),phantom() %~~% phantom(),italic(N[temporal]))), size = "Spatial sample size") +
  geom_ribbon(data = conf3, mapping = aes(x = pred_data, y = mu.mean3,
                                          xmin = min(pred_data), xmax = max(pred_data),
                                          ymin = ci.mean3[1,],
                                          ymax = ci.mean3[2,]),
              fill = "#31a354",
              alpha = 0.1)
p3

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ## RICHNESS - FOREST BIRDS - SUBSET ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("Output_TOsub_Poisson.RData")

# Extract parameters for ggplot
b_space <- summary(stanfit, pars = "b_space") # spatial slopes
b_time <- summary(stanfit, pars = "b_time") # temporal slopes
a <- summary(stanfit, pars = "a") # intercept

# Extract the slopes into a response and predictor variable (space = x, time = y)
x <- b_space$summary[,1]
y <- b_time$summary[,1]

bs <- rstan::extract(stanfit, "b_space") # extract spatial slopes
bt <- rstan::extract(stanfit, "b_time") # extract temporal slopes

b_space <- bs$b_space # assign to a referenceable object
b_time <- bt$b_time



# Set up fake data
pred_data = seq(from = (min(x) - 0.05), to = (max(x) + 0.05), length.out = 200 )

# Initialize
niterations = 8000
N = 200
intercept <- vector("list")
slope <- vector("list")
pred_lines <- vector("list")
mu <- matrix(nrow=8000, ncol=200)

# The code below cycles through every MCMC iteration (total of 8000) and calculates correlation of b_space (across 33 regions) and b_time (across 33 regions)
# Then, it predicts plottable relationship using the intercept & slope for each iteration
for (i in 1:niterations){
  
  mlm = lm(b_time[i,] ~ b_space[i,]) # linear regression between time and space
  
  intercept[i] = mlm$coefficients[[1]] # extract intercept
  slope[i] = mlm$coefficients[[2]] # extract slope between space-time
  
  mu[i,] <- sapply(pred_data,  function(x) mean( unlist(intercept[i]) + unlist(slope[i]) * x ) ) # derive plottable values for mean relationship between time and space
  
}

mu.mean4 <- apply( mu, 2, mean ) # mean of distribution of y for each value of x
ci.mean4 <- apply( mu, 2, PI, prob = 0.95 ) # calculate confidence interval

# Plot in R
plot(x, y)
lines( pred_data, mu.mean4, col = "#2c7bb6", lwd = 1.5)
shade( ci.mean4, pred_data)


#### GGPLOT ####
## Now move onto ggplot and just feed the intercept & slope in; compare to above (which is the correct way of doing it)
r <- data.frame(x, y)
r$Region <- seq(1:31)

d <- read.csv("~/manuscript/FinalDataset_TO.csv")

# Set up a column that contains info on how many spatial sites are associated with each temporal site
spatial <- d %>% filter(space.time == 2)
spatialSites <- spatial %>% group_by(ref) %>% summarize(nsites = n_distinct(RouteNumber)) %>% select(ref, nsites)
spatialSites$Region <- as.integer(as.factor(spatialSites$ref))

# Merge the column into the main dataframe
r <- merge(r, spatialSites, by = "Region")

# Take the mean of all intercepts and slopes calculated by the 8000 regressions between time & space
int_avg <- mean(unlist(intercept))
slope_avg <- mean(unlist(slope))
conf4 <- data.frame(pred_data, mu.mean4)

# Extract parameters for ggplot
b_space <- summary(stanfit, pars = "b_space") # spatial slopes
b_time <- summary(stanfit, pars = "b_time") # temporal slopes
a <- summary(stanfit, pars = "a") # intercept

# Extract the slopes into a response and predictor variable (space = x, time = y)
x <- b_space$summary[,1]
y <- b_time$summary[,1]


## all regions
p4 <- ggplot(r, mapping = aes(x, y)) +
  geom_point(
    colour = "#addd8e",
    alpha = 0.8,
    size = 3,
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    alpha = 0.5,
    size = 0.5
  ) +
  geom_abline(
    intercept = int_avg,
    slope = slope_avg,
    colour = "#addd8e",
    size = 1
  ) +
  labs(
    x = "Spatial slope", 
    y = "Temporal slope",
    size = 4
  ) +
  theme_classic()

p4 <- p4 + theme(plot.margin = unit(c(0,1,1,1),"cm"),
                 axis.title = element_text(size = 14), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
                 axis.text = element_text(size = 12)) + labs(title = "") +
  geom_ribbon(data = conf4, mapping = aes(x = pred_data, y = mu.mean4,
                                          xmin = min(pred_data), xmax = max(pred_data),
                                          ymin = ci.mean4[1,],
                                          ymax = ci.mean4[2,]),
              fill = "#addd8e",
              alpha = 0.1)
p4


all <- ggarrange(p3,
                 p1 + theme(legend.position="bottom"), 
                 p4,
                 p2 + theme(legend.position="bottom"),
                 ncol = 2, nrow = 2)

all

ggsave(filename = "Abundance_SpaceTimeCorrelation.png", device = "png", plot = all,
       width = 30, height = 30, units = "cm")

