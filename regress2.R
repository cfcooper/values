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
delta$rucc <- as.numeric(delta$rucc)
nondelta$rucc <- as.numeric(nondelta$rucc)
delta$rural <- ifelse(delta$rucc >= 4, 1, 0)
nondelta$rural <- ifelse(nondelta$rucc >= 4, 1, 0)


## value models ---------------------------------------------------------------

affordable <- lm(afford ~ Q8 + Q5 + hispanic + native + black + rucc, data = delta)
summary(affordable)

affordable <- lm(afford ~ Q8 + Q5 + hispanic + native + black + hh + rucc, data = nondelta)
summary(affordable)


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

#adjusted r2
adjusted_r2 <- sapply(regressions, function(model) {
  summary(model)$adj.r.squared
})

# Combine into a data frame
adjusted_r2_df <- data.frame(
  Regression = paste0("Regression_", 1:length(regressions)),
  Adjusted_R2 = adjusted_r2
)

abs_delta <- merge(pvalue_long, adjusted_r2_df)

write.csv(abs_delta, "cleaneddata/regress/abs_delta.csv")

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


#adjusted r2
adjusted_r2 <- sapply(regressions, function(model) {
  summary(model)$adj.r.squared
})

# Combine into a data frame
adjusted_r2_df <- data.frame(
  Regression = paste0("Regression_", 1:length(regressions)),
  Adjusted_R2 = adjusted_r2
)

abs_nondelta <- merge(pvalue_long, adjusted_r2_df)

write.csv(abs_nondelta, "cleaneddata/regress/abs_nondelta.csv")


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


#adjusted r2
adjusted_r2 <- sapply(regressions, function(model) {
  summary(model)$adj.r.squared
})

# Combine into a data frame
adjusted_r2_df <- data.frame(
  Regression = paste0("Regression_", 1:length(regressions)),
  Adjusted_R2 = adjusted_r2
)

percent_expend_delta <- merge(pvalue_long, adjusted_r2_df)

write.csv(percent_expend_delta, "cleaneddata/regress/percent_expend_delta.csv")


##round 2 nondelta --------------------------------------------
regressions <- list(
  lm(supermarketwhole_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(supermarketfood_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(healthfood_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(convenience_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(online_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(discount_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(smallstore_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(farmmarket_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(directfarm_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(foodbox_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(mealkit_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(market_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(chainrest_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(localrest_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
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


#adjusted r2
adjusted_r2 <- sapply(regressions, function(model) {
  summary(model)$adj.r.squared
})

# Combine into a data frame
adjusted_r2_df <- data.frame(
  Regression = paste0("Regression_", 1:length(regressions)),
  Adjusted_R2 = adjusted_r2
)

percent_expend_nondelta <- merge(pvalue_long, adjusted_r2_df)

write.csv(percent_expend_nondelta, "cleaneddata/regress/percent_expend_nondelta.csv")


##round 3 delta --------------------------------------------
regressions <- list(
  lm(supermarketwhole_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(supermarketfood_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(healthfood_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(convenience_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(online_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(discount_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(smallstore_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(farmmarket_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(directfarm_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(foodbox_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(mealkit_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(market_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(chainrest_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = delta),
  lm(localrest_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
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


#adjusted r2
adjusted_r2 <- sapply(regressions, function(model) {
  summary(model)$adj.r.squared
})

# Combine into a data frame
adjusted_r2_df <- data.frame(
  Regression = paste0("Regression_", 1:length(regressions)),
  Adjusted_R2 = adjusted_r2
)

percent_expend_delta <- merge(pvalue_long, adjusted_r2_df)

write.csv(percent_expend_delta, "cleaneddata/regress/percent_income_delta.csv")


##round 3 nondelta --------------------------------------------
regressions <- list(
  lm(supermarketwhole_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(supermarketfood_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(healthfood_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(convenience_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(online_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(discount_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(smallstore_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(farmmarket_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(directfarm_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(foodbox_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(mealkit_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(market_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(chainrest_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access, data = nondelta),
  lm(localrest_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
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


#adjusted r2
adjusted_r2 <- sapply(regressions, function(model) {
  summary(model)$adj.r.squared
})

# Combine into a data frame
adjusted_r2_df <- data.frame(
  Regression = paste0("Regression_", 1:length(regressions)),
  Adjusted_R2 = adjusted_r2
)

percent_expend_nondelta <- merge(pvalue_long, adjusted_r2_df)

write.csv(percent_expend_nondelta, "cleaneddata/regress/percent_income_nondelta.csv")




##round 4 delta --------------------------------------------
regressions <- list(
  lm(supermarketwhole_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(supermarketfood_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(healthfood_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(convenience_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(online_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(discount_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(smallstore_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(farmmarket_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(directfarm_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(foodbox_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(mealkit_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(market_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(chainrest_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(localrest_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta)
)



# Extract p-values for each regression
pvalues <- lapply(regressions, function(model) {
  summary(model)$coefficients[, "Pr(>|t|)"]
})


pvalue_df <- do.call(rbind, pvalues)
colnames(pvalue_df) <- c("(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
                         "healthy", "social_resp", "access", "rural","Q5")
pvalue_df <- as.data.frame(pvalue_df)

# Add a column for the regression number
pvalue_df$Regression <- paste0("Regression_", 1:length(regressions))

# Arrange the data into a long format where each coefficient is its own column
pvalue_long <- pvalue_df %>%
  select(Regression, "(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
         "healthy", "social_resp", "access", "rural", "Q5") %>%
  arrange(Regression)

#adjusted r2
adjusted_r2 <- sapply(regressions, function(model) {
  summary(model)$adj.r.squared
})

# Combine into a data frame
adjusted_r2_df <- data.frame(
  Regression = paste0("Regression_", 1:length(regressions)),
  Adjusted_R2 = adjusted_r2
)

abs_delta <- merge(pvalue_long, adjusted_r2_df)

write.csv(abs_delta, "cleaneddata/regress/abs_delta_r2.csv")

# Extract coefficients for each regression
coefficients <- lapply(regressions, function(model) {
  coef(model)  # Extract coefficients
})

coefficient_df <- do.call(rbind, coefficients)
colnames(coefficient_df) <- c("(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
                              "healthy", "social_resp", "access", "rural", "Q5")
coefficient_df <- as.data.frame(coefficient_df)

# Add a column for the regression number
coefficient_df$Regression <- paste0("Regression_", 1:length(regressions))

# Arrange the data into a long format where each coefficient is its own column
coeffcient_long <- coefficient_df %>%
  select(Regression, "(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
         "healthy", "social_resp", "access", "rural", "Q5") %>%
  arrange(Regression)

write.csv(coeffcient_long, "cleaneddata/regress/abs_delta_c2.csv")

##round 4 nondelta --------------------------------------------
regressions <- list(
  lm(supermarketwhole_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = nondelta),
  lm(supermarketfood_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = nondelta),
  lm(healthfood_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = nondelta),
  lm(convenience_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = nondelta),
  lm(online_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = nondelta),
  lm(discount_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = nondelta),
  lm(smallstore_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = nondelta),
  lm(farmmarket_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = nondelta),
  lm(directfarm_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = nondelta),
  lm(foodbox_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = nondelta),
  lm(mealkit_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = nondelta),
  lm(market_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = nondelta),
  lm(chainrest_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = nondelta),
  lm(localrest_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = nondelta)
)



# Extract p-values for each regression
pvalues <- lapply(regressions, function(model) {
  summary(model)$coefficients[, "Pr(>|t|)"]
})


pvalue_df <- do.call(rbind, pvalues)
colnames(pvalue_df) <- c("(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
                         "healthy", "social_resp", "access", "rural", "Q5")
pvalue_df <- as.data.frame(pvalue_df)

# Add a column for the regression number
pvalue_df$Regression <- paste0("Regression_", 1:length(regressions))

# Arrange the data into a long format where each coefficient is its own column
pvalue_long <- pvalue_df %>%
  select(Regression, "(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
         "healthy", "social_resp", "access", "rural", "Q5") %>%
  arrange(Regression)

#adjusted r2
adjusted_r2 <- sapply(regressions, function(model) {
  summary(model)$adj.r.squared
})

# Combine into a data frame
adjusted_r2_df <- data.frame(
  Regression = paste0("Regression_", 1:length(regressions)),
  Adjusted_R2 = adjusted_r2
)

abs_nondelta <- merge(pvalue_long, adjusted_r2_df)

write.csv(abs_nondelta, "cleaneddata/regress/abs_nondelta_r2.csv")

# Extract coefficients for each regression
coefficients <- lapply(regressions, function(model) {
  coef(model)  # Extract coefficients
})

coefficient_df <- do.call(rbind, coefficients)
colnames(coefficient_df) <- c("(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
                         "healthy", "social_resp", "access", "rural", "Q5")
coefficient_df <- as.data.frame(coefficient_df)

# Add a column for the regression number
coefficient_df$Regression <- paste0("Regression_", 1:length(regressions))

# Arrange the data into a long format where each coefficient is its own column
coeffcient_long <- coefficient_df %>%
  select(Regression, "(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
         "healthy", "social_resp", "access", "rural", "Q5") %>%
  arrange(Regression)

write.csv(coeffcient_long, "cleaneddata/regress/abs_nondelta_c2.csv")



##round 4_1 nondelta --------------------------------------------
regressions <- list(
  lm(supermarketwhole_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc + Q5 + hispanic + native + black + Q49, data = nondelta),
  lm(supermarketfood_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc + Q5 + hispanic + native + black + Q49, data = nondelta),
  lm(healthfood_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc + Q5 + hispanic + native + black + Q49, data = nondelta),
  lm(convenience_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc + Q5 + hispanic + native + black + Q49, data = nondelta),
  lm(online_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc + Q5 + hispanic + native + black + Q49, data = nondelta),
  lm(discount_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc + Q5 + hispanic + native + black + Q49, data = nondelta),
  lm(smallstore_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc + Q5 + hispanic + native + black + Q49, data = nondelta),
  lm(farmmarket_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc + Q5 + hispanic + native + black + Q49, data = nondelta),
  lm(directfarm_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc + Q5 + hispanic + native + black + Q49, data = nondelta),
  lm(foodbox_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc + Q5 + hispanic + native + black + Q49, data = nondelta),
  lm(mealkit_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc + Q5 + hispanic + native + black + Q49, data = nondelta),
  lm(market_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc + Q5 + hispanic + native + black + Q49, data = nondelta),
  lm(chainrest_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc + Q5 + hispanic + native + black + Q49, data = nondelta),
  lm(localrest_expend ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc + Q5 + hispanic + native + black + Q49, data = nondelta)
)



# Extract p-values for each regression
pvalues <- lapply(regressions, function(model) {
  summary(model)$coefficients[, "Pr(>|t|)"]
})


pvalue_df <- do.call(rbind, pvalues)
colnames(pvalue_df) <- c("(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
                         "healthy", "social_resp", "access", "rucc","Q5", "hispanic", "native","black","Q49")
pvalue_df <- as.data.frame(pvalue_df)

# Add a column for the regression number
pvalue_df$Regression <- paste0("Regression_", 1:length(regressions))

# Arrange the data into a long format where each coefficient is its own column
pvalue_long <- pvalue_df %>%
  select(Regression, "(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
         "healthy", "social_resp", "access", "rucc","Q5", "hispanic", "native","black","Q49") %>%
  arrange(Regression)

#adjusted r2
adjusted_r2 <- sapply(regressions, function(model) {
  summary(model)$adj.r.squared
})

# Combine into a data frame
adjusted_r2_df <- data.frame(
  Regression = paste0("Regression_", 1:length(regressions)),
  Adjusted_R2 = adjusted_r2
)

abs_nondelta <- merge(pvalue_long, adjusted_r2_df)

write.csv(abs_nondelta, "cleaneddata/regress/abs_nondelta_r2.csv")

##round 5 delta --------------------------------------------
regressions <- list(
  lm(supermarketwhole_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(supermarketfood_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(healthfood_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(convenience_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(online_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(discount_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(smallstore_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(farmmarket_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(directfarm_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(foodbox_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(mealkit_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(market_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(chainrest_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta),
  lm(localrest_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rural + Q5, data = delta)
)



# Extract p-values for each regression
pvalues <- lapply(regressions, function(model) {
  summary(model)$coefficients[, "Pr(>|t|)"]
})


pvalue_df <- do.call(rbind, pvalues)
colnames(pvalue_df) <- c("(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
                         "healthy", "social_resp", "access","rural","Q5")
pvalue_df <- as.data.frame(pvalue_df)

# Add a column for the regression number
pvalue_df$Regression <- paste0("Regression_", 1:length(regressions))

# Arrange the data into a long format where each coefficient is its own column
pvalue_long <- pvalue_df %>%
  select(Regression, "(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
         "healthy", "social_resp", "access","rural","Q5") %>%
  arrange(Regression)


#adjusted r2
adjusted_r2 <- sapply(regressions, function(model) {
  summary(model)$adj.r.squared
})

# Combine into a data frame
adjusted_r2_df <- data.frame(
  Regression = paste0("Regression_", 1:length(regressions)),
  Adjusted_R2 = adjusted_r2
)

percent_expend_delta <- merge(pvalue_long, adjusted_r2_df)

write.csv(percent_expend_delta, "cleaneddata/regress/percent_expend_delta_r.csv")



##round 5 nondelta --------------------------------------------
regressions <- list(
  lm(supermarketwhole_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(supermarketfood_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(healthfood_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(convenience_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(online_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(discount_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(smallstore_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(farmmarket_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(directfarm_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(foodbox_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(mealkit_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(market_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(chainrest_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(localrest_expend_t ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta)
)



# Extract p-values for each regression
pvalues <- lapply(regressions, function(model) {
  summary(model)$coefficients[, "Pr(>|t|)"]
})


pvalue_df <- do.call(rbind, pvalues)
colnames(pvalue_df) <- c("(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
                         "healthy", "social_resp", "access","rucc")
pvalue_df <- as.data.frame(pvalue_df)

# Add a column for the regression number
pvalue_df$Regression <- paste0("Regression_", 1:length(regressions))

# Arrange the data into a long format where each coefficient is its own column
pvalue_long <- pvalue_df %>%
  select(Regression, "(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
         "healthy", "social_resp", "access","rucc") %>%
  arrange(Regression)


#adjusted r2
adjusted_r2 <- sapply(regressions, function(model) {
  summary(model)$adj.r.squared
})

# Combine into a data frame
adjusted_r2_df <- data.frame(
  Regression = paste0("Regression_", 1:length(regressions)),
  Adjusted_R2 = adjusted_r2
)

percent_expend_nondelta <- merge(pvalue_long, adjusted_r2_df)

write.csv(percent_expend_nondelta, "cleaneddata/regress/percent_expend_nondelta_r.csv")




##round 6 delta --------------------------------------------
regressions <- list(
  lm(supermarketwhole_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = delta),
  lm(supermarketfood_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = delta),
  lm(healthfood_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = delta),
  lm(convenience_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = delta),
  lm(online_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = delta),
  lm(discount_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = delta),
  lm(smallstore_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = delta),
  lm(farmmarket_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = delta),
  lm(directfarm_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = delta),
  lm(foodbox_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = delta),
  lm(mealkit_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = delta),
  lm(market_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = delta),
  lm(chainrest_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = delta),
  lm(localrest_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = delta)
)



# Extract p-values for each regression
pvalues <- lapply(regressions, function(model) {
  summary(model)$coefficients[, "Pr(>|t|)"]
})


pvalue_df <- do.call(rbind, pvalues)
colnames(pvalue_df) <- c("(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
                         "healthy", "social_resp", "access", "rucc")
pvalue_df <- as.data.frame(pvalue_df)

# Add a column for the regression number
pvalue_df$Regression <- paste0("Regression_", 1:length(regressions))

# Arrange the data into a long format where each coefficient is its own column
pvalue_long <- pvalue_df %>%
  select(Regression, "(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
         "healthy", "social_resp", "access", "rucc") %>%
  arrange(Regression)


#adjusted r2
adjusted_r2 <- sapply(regressions, function(model) {
  summary(model)$adj.r.squared
})

# Combine into a data frame
adjusted_r2_df <- data.frame(
  Regression = paste0("Regression_", 1:length(regressions)),
  Adjusted_R2 = adjusted_r2
)

percent_expend_delta <- merge(pvalue_long, adjusted_r2_df)

write.csv(percent_expend_delta, "cleaneddata/regress/percent_income_delta_r.csv")


##round 6 nondelta --------------------------------------------
regressions <- list(
  lm(supermarketwhole_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(supermarketfood_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(healthfood_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(convenience_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(online_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(discount_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(smallstore_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(farmmarket_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(directfarm_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(foodbox_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(mealkit_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(market_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(chainrest_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta),
  lm(localrest_expend_p ~ income_weekly + locally_grown + organic + local_econ + afford +
       healthy + social_resp + access + rucc, data = nondelta)
)



# Extract p-values for each regression
pvalues <- lapply(regressions, function(model) {
  summary(model)$coefficients[, "Pr(>|t|)"]
})


pvalue_df <- do.call(rbind, pvalues)
colnames(pvalue_df) <- c("(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
                         "healthy", "social_resp", "access", "rucc")
pvalue_df <- as.data.frame(pvalue_df)

# Add a column for the regression number
pvalue_df$Regression <- paste0("Regression_", 1:length(regressions))

# Arrange the data into a long format where each coefficient is its own column
pvalue_long <- pvalue_df %>%
  select(Regression, "(Intercept)", "income_weekly", "locally_grown", "organic", "local_econ", "afford",
         "healthy", "social_resp", "access", "rucc") %>%
  arrange(Regression)


#adjusted r2
adjusted_r2 <- sapply(regressions, function(model) {
  summary(model)$adj.r.squared
})

# Combine into a data frame
adjusted_r2_df <- data.frame(
  Regression = paste0("Regression_", 1:length(regressions)),
  Adjusted_R2 = adjusted_r2
)

percent_expend_nondelta <- merge(pvalue_long, adjusted_r2_df)

write.csv(percent_expend_nondelta, "cleaneddata/regress/percent_income_nondelta_r.csv")


