## load up stuff
library(tidyverse)
library(cmdstanr)
library(shinystan)
library(rstan)
library(bayesplot)

rm(list = ls())
gc()

d <- read.csv("~/space-for-time/FinalDataset_RFsub.csv")

d <- d[!is.na(d$Richness),] # do only if using routes, not stops

write.csv(d, "~/space-for-time/FinalDataset_RFsub.csv")

# Initial data plotting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hist(d$Richness)
min(d$Richness)
max(d$Richness)
mean(d$Richness)
median(d$Richness)

## Set up the data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Create space and time indicators
d$space <- d$space.time
d$space[which(d$space == 1)] <- 0
d$space[which(d$space == 2)] <- 1

d$time <- d$space.time
d$time[which(d$time == 2)] <- 0
d$time[which(d$time == 1)] <- 1
d$ObsN <- as.integer(as.factor(d$ObsN))

d$Region <- as.integer(as.factor(d$ref)) # Create integer categorical for 'region' or space-time comparison

x.seq = seq( from=min(d$Forest.cover), to = max(d$Forest.cover), length.out = 100 )

d_slim <- list(
  ncounts = nrow(d),
  nreg = length(unique(d$Region)),
  nobs = length(unique(d$ObsN)),
  nst = 2,
  nstops = length(unique(d$NumStops)),
  stops = d$NumStops,
  ndata = 100,
  
  ta = d$TA, # turn on if running abundance model
  #richness = d$Richness, # turn on if running richness model
  spacetime = d$space.time,
  space = d$space,
  time = d$time,
  reg = d$Region,
  
  pforest = d$Forest.cover,
  obs = as.integer(as.factor(d$ObsN)),
  nfirstobs = 2,
  firstobs = d$FirstObs,
  xseq = (x.seq-mean(d$Forest.cover))/sd(d$Forest.cover)
)

# Compile the model in cmdstan  ~~~~~~~~~~~~~~~~~~~
# file <- file.path ("AbundanceRegression.stan") # turn on if running abundance model
file <- file.path("~/space-for-time/Rstan models/AbundanceRegressionPoisson.stan") # turn on if running richness model
mod <- cmdstan_model(file, pedantic = TRUE)
check_cmdstan_toolchain(fix = TRUE)


# Run the model ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fit <- mod$sample(
  data = d_slim,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000,
  parallel_chains = 2,
  show_messages = TRUE,
  adapt_delta = 0.99,
  max_treedepth = 18,
  step_size = 0.01,
  output_dir = "~/space-time/cmdstan_output_files/"
)


# create a stanfit S4 object 
stanfit <- rstan::read_stan_csv(fit$output_files())
save(stanfit, file =  "Output_TOsub_Final.RData")

y <- d$Richness
y <- d$TA

# Load up in shinystan for convergence diagnostics & posterior predictive / assumptions
shinyfit <- as.shinystan(stanfit)
launch_shinystan(shinyfit)

# Posterior predictive check using bayesplot()
y_rep <- as.matrix(stanfit, pars = "y_rep")
ppc_dens_overlay(y = y, yrep = y_rep)