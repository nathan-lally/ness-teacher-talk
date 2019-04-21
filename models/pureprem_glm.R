##################################################
#### NESS Teacher talk model fitting for demo ####
#### Tweedie GLM                              ####
#### Nathan Lally, 4/13/2019                  ####
##################################################

#### Load packages ####
library(tidyverse)
library(CASdatasets)
library(caret)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#### fetch claims data and clean ####
data(freMPL1)
df <- as_tibble(freMPL1) %>%
  select(Exposure,ClaimAmount,ClaimInd,LicAge,VehAge,Gender,VehUsage,DrivAge,VehBody,VehMaxSpeed) %>% 
  filter(ClaimAmount>=0)
rm(freMPL1)
df$binAge <- cut(df$DrivAge, 10, include.lowest = TRUE)

#### fit tweedie GLM ####
# stan data
dums <- dummyVars(data=df, formula = ~ Gender + VehUsage + binAge) # create design matrix
X <- predict(dums, newdata=df)
moddat <- list(N=nrow(X), # data in list for stan program
               Y=df$ClaimAmount,
               P=ncol(X),
               M=5,
               lnexposure = log(df$Exposure),
               X=X)
# fit
mod <- stan_model(file="tweedie_lasso.stan") # compile model
f1 <- sampling(mod, data=moddat, chains=4, iter=1000) # sample
rstan::traceplot(f1, pars=c("omega_0","omega","phi","theta")) # convergence diagnostics
# make predictions
omega_0 <- mean(unlist(rstan::extract(f1, pars="omega_0"))) # posterior means of regression coefficients
omega <- apply(rstan::extract(f1, pars="omega")[[1]], 2, mean)
df$tweedie_loss_cost <- exp(omega_0 + X %*% omega) # expected pure premium
# save results
save.image(file = "tweedieglm.RData")
