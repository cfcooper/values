
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
#zcta <- read.csv("zcta/zip_density.csv")
regionCPI <- read.csv("regionalcpi.csv")


nondelta$income_weekly <- nondelta$income/52
delta$income_weekly <- delta$income/52



# merge data -------------------------------------------------

#delta$rucc <- as.factor(delta$rucc)
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
names(fulldata)[names(fulldata) == "Q51"] <- "zip_code"
#fulldata <- merge(fulldata, zcta,sort = FALSE)
fulldata <- merge(fulldata, regionCPI, sort = FALSE)


fulldata$zip_code <- as.numeric(fulldata$zip_code)
fulldata$healthfood_expend <- as.integer(fulldata$healthfood_expend)
fulldata$farmmarket_expend <- as.integer(fulldata$farmmarket_expend)
fulldata$supermarketwhole_expend <- as.integer(fulldata$supermarketwhole_expend)
fulldata$supermarketfood_expend <- as.integer(fulldata$supermarketfood_expend)
fulldata$convenience_expend <- as.integer(fulldata$convenience_expend)
fulldata$online_expend <- as.integer(fulldata$online_expend)
fulldata$discount_expend <- as.integer(fulldata$discount_expend)
fulldata$smallstore_expend <- as.integer(fulldata$smallstore_expend)
fulldata$directfarm_expend <- as.integer(fulldata$directfarm_expend)
fulldata$foodbox_expend <- as.integer(fulldata$foodbox_expend)
fulldata$mealkit_expend <- as.integer(fulldata$mealkit_expend)

fulldata$income_adj <- fulldata$income_weekly/fulldata$adj_CPI



# models

m1 <- zeroinfl(healthfood_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + income_adj + region_Delta | rural + income_adj + region_Delta, data = fulldata, dist = "negbin")
summary(m1)

m2 <- zeroinfl(farmmarket_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + income_adj + region_Delta | rural + income_adj + region_Delta, data = fulldata, dist = "negbin")
summary(m2)

m3 <- zeroinfl(supermarketwhole_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + income_adj + region_Delta | rural + income_adj + region_Delta, data = fulldata, dist = "negbin")
summary(m3)



# Access coefficients

coefficients_count <- summary(m10)$coefficients$count[, "Estimate"]  # Coefficients for the count model

coefficients_zero <- summary(m10)$coefficients$zero[, "Estimate"]  # Coefficients for the zero-inflation model



# Export coefficients (e.g., to a CSV)

write.csv(data.frame(variable = names(coefficients_count), count_coef = coefficients_count), file = "zinb_coefficients.csv", row.names = FALSE)
write.csv(data.frame(variable = names(coefficients_zero), count_coef = coefficients_zero), file = "zinb_coefficients.csv", row.names = FALSE)


m4 <- zeroinfl(supermarketfood_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + income_adj + region_Delta | rural + income_adj + region_Delta, data = fulldata, dist = "negbin")
summary(m4)

m5 <- zeroinfl(convenience_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + income_adj + region_Delta | rural + income_adj + region_Delta, data = fulldata, dist = "negbin")
summary(m5)

m6 <- zeroinfl(online_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + income_adj + region_Delta | rural + income_adj + region_Delta, data = fulldata, dist = "negbin")
summary(m6)

m7 <- zeroinfl(discount_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + income_adj + region_Delta | rural + income_adj + region_Delta, data = fulldata, dist = "negbin")
summary(m7)

m8 <- zeroinfl(smallstore_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + income_adj + region_Delta | rural + income_adj + region_Delta, data = fulldata, dist = "negbin")
summary(m8)

m9 <- zeroinfl(directfarm_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + income_adj + region_Delta | rural + income_adj + region_Delta, data = fulldata, dist = "negbin")
summary(m9)

m10 <- zeroinfl(foodbox_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + income_adj + region_Delta | rural + income_adj + region_Delta, data = fulldata, dist = "negbin")
summary(m10)

m11 <- zeroinfl(mealkit_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + income_adj + region_Delta | rural + income_adj + region_Delta, data = fulldata, dist = "negbin")
summary(m11)

# interaction model test -------------------------------------------------

a1 <- zeroinfl(healthfood_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + income_adj + region_Delta + income_adj*region_Delta | 
                 rural + income_adj + region_Delta, data = fulldata, dist = "negbin")
summary(a1)

