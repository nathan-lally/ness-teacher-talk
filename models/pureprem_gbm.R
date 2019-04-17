##################################################
#### NESS Teacher talk model fitting for demo ####
#### Tweedie GBM                              ####
#### Nathan Lally, 4/13/2019                  ####
##################################################

#### Load packages ####
library(tidyverse)
library(cplm)
library(CASdatasets)
library(caret)
library(TDboost)


#### fetch claims data and clean ####
data(freMPL1)
df <- as_tibble(freMPL1) %>%
  select(Exposure,ClaimAmount,LicAge,VehAge,Gender,VehUsage,DrivAge,VehBody,VehMaxSpeed) %>% 
  filter(ClaimAmount>=0) %>% 
  mutate(VehAge = case_when(VehAge=="0" ~ 0,
                   VehAge=="1" ~ 1,
                   VehAge=="2" ~ 2,
                   VehAge=="3" ~ 3,
                   VehAge=="4" ~ 4,
                   VehAge=="5" ~ 5,
                   VehAge=="6-7" ~ 6,
                   VehAge=="8-9" ~ 7,
                   VehAge=="10+" ~ 8,
                   TRUE ~ as.numeric(VehAge)))
  
rm(freMPL1)

#### fit tweedie GBM ####
#tuning grid
tune.grid <- expand.grid(alpha=seq(1.4,1.9,0.1), 
                         interaction.depth = 1:3, 
                         n.minobsinnode = c(20),
                         shrinkage = c(1e-4, 1e-3,1e-2,0.1))
tune.res <- data.frame(trees = rep(NA, nrow(tune.grid)), cv.err=NA)
for(i in 1:(nrow(tune.grid))){
  tb <- TDboost(data=df, 
                formula = ClaimAmount ~ LicAge + VehAge + Gender + VehUsage + DrivAge + VehBody + VehMaxSpeed + offset(log(Exposure)),
                cv.folds = 10,
                n.trees = 500,
                distribution = list(name="EDM",alpha=tune.grid$alpha[i]),
                interaction.depth = tune.grid$interaction.depth[i],
                n.minobsinnode = tune.grid$n.minobsinnode[i],
                shrinkage = tune.grid$shrinkage[i])
  tune.res[i,] <- c(which(tb$cv.error==min(tb$cv.error)), min(tb$cv.error))
}
tune.grid <- cbind(tune.grid, tune.res)
best.fit <- tune.grid[tune.grid$cv.err==min(tune.grid$cv.err),]
# fit best model
tb <- TDboost(data=df, 
              formula = ClaimAmount ~ LicAge + VehAge + Gender + VehUsage + DrivAge + VehBody + VehMaxSpeed + offset(log(Exposure)),
              cv.folds = 0,
              n.trees = 300,
              distribution = list(name="EDM",alpha=best.fit$alpha),
              interaction.depth = best.fit$interaction.depth,
              n.minobsinnode = best.fit$n.minobsinnode,
              shrinkage = best.fit$shrinkage)
df$tweedie_loss_cost <- predict(tb, newdata=df, n.trees=300)
save.image(file = "tweediegbm.RData")
