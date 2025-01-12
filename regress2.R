 #regress 2


library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape)
library(magrittr)
library(formattable)


rm(list=ls()) # caution: this clears the environment

## read in dataframes -----------------------------------------------------------

delta <- readRDS("cleaneddata/delta.rds")
nondelta <- readRDS("cleaneddata/nondelta.rds")
write.csv(delta, "delta_raw.csv")

nondelta$income_weekly <- nondelta$income/52
delta$income_weekly <- delta$income/52


##round 1 delta --------------------------------------------
regressions <- list(
  lm(supermarketwhole_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(supermarketfood_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(healthfood_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(convenience_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(online_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(discount_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(smallstore_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(farmmarket_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(directfarm_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(foodbox_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(mealkit_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(market_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(chainrest_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(localrest_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta)
)



# Extract p-values for each regression
pvalues <- lapply(regressions, function(model) {
  summary(model)$coefficients[, "Pr(>|t|)"]
})


pvalue_df <- do.call(rbind, pvalues)
colnames(pvalue_df) <- c("(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
                         "healthy", "social_resp", "access")
pvalue_df <- as.data.frame(pvalue_df)

# Add a column for the regression number
pvalue_df$Regression <- paste0("Regression_", 1:length(regressions))

# Arrange the data into a long format where each coefficient is its own column
pvalue_long <- pvalue_df %>%
  select(Regression, "(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
         "healthy", "social_resp", "access") %>%
  arrange(Regression)

write.csv(pvalue_long, "cleaneddata/regress/abs_delta.csv")

##round 1 nondelta --------------------------------------------
regressions <- list(
  lm(supermarketwhole_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(supermarketfood_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(healthfood_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(convenience_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(online_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(discount_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(smallstore_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(farmmarket_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(directfarm_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(foodbox_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(mealkit_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(market_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(chainrest_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(localrest_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta)
)



# Extract p-values for each regression
pvalues <- lapply(regressions, function(model) {
  summary(model)$coefficients[, "Pr(>|t|)"]
})


pvalue_df <- do.call(rbind, pvalues)
colnames(pvalue_df) <- c("(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
                         "healthy", "social_resp", "access")
pvalue_df <- as.data.frame(pvalue_df)

# Add a column for the regression number
pvalue_df$Regression <- paste0("Regression_", 1:length(regressions))

# Arrange the data into a long format where each coefficient is its own column
pvalue_long <- pvalue_df %>%
  select(Regression, "(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
         "healthy", "social_resp", "access") %>%
  arrange(Regression)

write.csv(pvalue_long, "cleaneddata/regress/abs_nondelta.csv")


##round 2 delta --------------------------------------------
regressions <- list(
  lm(supermarketwhole_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(supermarketfood_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(healthfood_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(convenience_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(online_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(discount_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(smallstore_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(farmmarket_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(directfarm_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(foodbox_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(mealkit_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(market_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(chainrest_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(localrest_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta)
)



# Extract p-values for each regression
pvalues <- lapply(regressions, function(model) {
  summary(model)$coefficients[, "Pr(>|t|)"]
})


pvalue_df <- do.call(rbind, pvalues)
colnames(pvalue_df) <- c("(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
                         "healthy", "social_resp", "access")
pvalue_df <- as.data.frame(pvalue_df)

# Add a column for the regression number
pvalue_df$Regression <- paste0("Regression_", 1:length(regressions))

# Arrange the data into a long format where each coefficient is its own column
pvalue_long <- pvalue_df %>%
  select(Regression, "(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
         "healthy", "social_resp", "access") %>%
  arrange(Regression)

write.csv(pvalue_long, "cleaneddata/regress/percent_expend_delta.csv")