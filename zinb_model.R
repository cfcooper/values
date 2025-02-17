
## zinb model ##


library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape)
library(magrittr)
library(formattable)
library(MASS)
library(pscl)
library(boot)



rm(list=ls()) # caution: this clears the environment

## read in dataframes -----------------------------------------------------------

delta <- readRDS("cleaneddata/delta.rds")
nondelta <- readRDS("cleaneddata/nondelta.rds")

# delta -------------------------------------------------

delta$rucc <- as.factor(delta$rucc)
delta$Q8 <- as.factor(delta$Q8)
delta$afford <- as.factor(delta$afford)
delta$healthy <- as.factor(delta$healthy)
delta$access <- as.factor(delta$access)
delta$locally_grown <- as.factor(delta$locally_grown)
delta$local_econ <- as.factor(delta$local_econ)
delta$social_resp <- as.factor(delta$social_resp)
delta$organic <- as.factor(delta$organic)

delta$healthfood_expend <- as.integer(delta$healthfood_expend)

delta_sum <- delta %>% group_by(Q8) %>%
  summarise(count = n())

delta$region_Delta <- 1
nondelta$region_Delta <- 0


rbind.match.columns <- function(input1, input2) {
  n.input1 <- ncol(input1)
  n.input2 <- ncol(input2)
  
  if (n.input2 < n.input1) {
    TF.names <- which(names(input2) %in% names(input1))
    column.names <- names(input2[, TF.names])
  } else {
    TF.names <- which(names(input1) %in% names(input2))
    column.names <- names(input1[, TF.names])
  }
  
  return(rbind(input1[, column.names], input2[, column.names]))
}

fulldata <- rbind.match.columns(delta, nondelta)

fulldata$healthfood_expend <- as.integer(fulldata$healthfood_expend)



# models

m1 <- zeroinfl(healthfood_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + region_Delta | rucc + Q8 + region_Delta, data = fulldata, dist = "negbin")
summary(m1)

# non delta -------------------------------------------------

nondelta$rucc <- as.factor(nondelta$rucc)
nondelta$Q8 <- as.factor(nondelta$Q8)
nondelta$afford <- as.factor(nondelta$afford)
nondelta$healthy <- as.factor(nondelta$healthy)
nondelta$access <- as.factor(nondelta$access)
nondelta$locally_grown <- as.factor(nondelta$locally_grown)
nondelta$local_econ <- as.factor(nondelta$local_econ)
nondelta$social_resp <- as.factor(nondelta$social_resp)
nondelta$organic <- as.factor(nondelta$organic)

nondelta$healthfood_expend <- as.integer(nondelta$healthfood_expend)

# models

m1 <- zeroinfl(healthfood_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic | rucc + Q8, data = nondelta, dist = "negbin")
summary(m1)






