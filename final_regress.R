
library(tidyr)
library(dplyr)
#library(ggplot2)
library(reshape)
library(magrittr)
library(formattable)



rm(list=ls()) # caution: this clears the environment

## read in dataframes -----------------------------------------------------------

delta <- readRDS("cleaneddata/delta.rds")
nondelta <- readRDS("cleaneddata/nondelta.rds")
#zcta <- read.csv("zcta/zip_density.csv")
regionCPI <- read.csv("regional_cpi.csv")
regionCPI <- subset(regionCPI, select = c(1:4,8))                  # subset: regional CPI


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

#delta$region_Delta <- 1
#nondelta$region_Delta <- 0


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

regions_sum <- fulldata %>% group_by(region) %>%
  summarise(count = n())
write.csv(regions_sum, "rfbc_counts.csv")

regions_sum2 <- fulldata %>% group_by(po_region) %>%
  summarise(count = n())

regions_sum3 <- fulldata %>% group_by(division) %>%
  summarise(count = n())



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
fulldata$market_expend <- as.integer(fulldata$market_expend)

fulldata$income_adj <- fulldata$income_weekly/fulldata$adj_CPI
fulldata$hh_size <- as.numeric(fulldata$Q49)



# Create binary columns for 'region'
region_dummies <- model.matrix(~ region - 1, data = fulldata)

# Optional: clean up column names
colnames(region_dummies) <- gsub("region", "", colnames(region_dummies))

# Combine binary columns with original data, excluding the original 'region' column
fulldata <- cbind(fulldata, region_dummies)

saveRDS(fulldata, "fulldata.rds")


test <- lm(supermarketwhole_expend ~ income_weekly + hh_size + rural + 
     locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
     Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + Northeast +
     Northwest + RioGrande + Southeast + Southwest, data = fulldata)
summary(test)




# expend tables -----------

fulldata$Q5 <- as.factor(fulldata$Q5)

sum_age_expend <- fulldata %>% group_by(Q5) %>%
  summarise(ch_1 = mean(supermarketwhole_expend),
            ch_2 = mean(supermarketfood_expend),
            ch_3 = mean(healthfood_expend),
            ch_4 = mean(convenience_expend),
            ch_5 = mean(online_expend),
            ch_6 = mean(discount_expend),
            ch_7 = mean(smallstore_expend),
            ch_8 = mean(farmmarket_expend),
            ch_9 = mean(directfarm_expend),
            ch_10 = mean(foodbox_expend),
            ch_11 = mean(mealkit_expend),
            ch_12 = mean(market_expend))

sum_age_expend <- fulldata %>% group_by(Q5) %>%
  summarise(ch_1 = mean(supermarketwhole_expend),
            ch_2 = mean(supermarketfood_expend),
            ch_3 = mean(healthfood_expend),
            ch_4 = mean(convenience_expend),
            ch_5 = mean(online_expend),
            ch_6 = mean(discount_expend),
            ch_7 = mean(smallstore_expend),
            ch_8 = mean(farmmarket_expend),
            ch_9 = mean(directfarm_expend),
            ch_10 = mean(foodbox_expend),
            ch_11 = mean(mealkit_expend),
            ch_12 = mean(market_expend))

sum_age_expend$sum <- rowSums(sum_age_expend[2:13])
write.csv(sum_age_expend, "cleaneddata/age_expend.csv")

# regressions RFBC ---------------------------------


regressions <- list(
  lm(supermarketwhole_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + Northeast +
       Northwest + RioGrande + Southeast + Southwest, data = fulldata),
  lm(supermarketfood_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + Northeast +
       Northwest + RioGrande + Southeast + Southwest, data = fulldata),
  lm(healthfood_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + Northeast +
       Northwest + RioGrande + Southeast + Southwest, data = fulldata),
  lm(convenience_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + Northeast +
       Northwest + RioGrande + Southeast + Southwest, data = fulldata),
  lm(online_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + Northeast +
       Northwest + RioGrande + Southeast + Southwest, data = fulldata),
  lm(discount_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + Northeast +
       Northwest + RioGrande + Southeast + Southwest, data = fulldata),
  lm(smallstore_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + Northeast +
       Northwest + RioGrande + Southeast + Southwest, data = fulldata),
  lm(farmmarket_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + Northeast +
       Northwest + RioGrande + Southeast + Southwest, data = fulldata),
  lm(directfarm_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + Northeast +
       Northwest + RioGrande + Southeast + Southwest, data = fulldata),
  lm(foodbox_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + Northeast +
       Northwest + RioGrande + Southeast + Southwest, data = fulldata),
  lm(mealkit_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + Northeast +
       Northwest + RioGrande + Southeast + Southwest, data = fulldata),
  lm(market_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + Northeast +
       Northwest + RioGrande + Southeast + Southwest, data = fulldata) )



# Extract p-values for each regression
pvalues <- lapply(regressions, function(model) {
  summary(model)$coefficients[, "Pr(>|t|)"]
})


pvalue_df <- do.call(rbind, pvalues)
colnames(pvalue_df) <- c("(Intercept)", "income_weekly", "hh_size", "rural", 
                         "Appalachia", "Delta", "GL_Midwest", "Heartland", "RemoteAreas",
                          "Northeast", "Northwest", "RioGrande", "Southeast", "Southwest",
                         "locally_grown", "organic", "local_econ", "afford",
                         "healthy", "social_resp", "access")
pvalue_df <- as.data.frame(pvalue_df)

# Add a column for the regression number
pvalue_df$Regression <- paste0("Regression_", 1:length(regressions))

# Arrange the data into a long format where each coefficient is its own column
pvalue_long <- pvalue_df %>%
  select(Regression, "(Intercept)", "income_weekly", "hh_size", "rural", 
         "Appalachia", "Delta", "GL_Midwest", "Heartland", "RemoteAreas",
         "Northeast", "Northwest", "RioGrande", "Southeast", "Southwest",
         "locally_grown", "organic", "local_econ", "afford",
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

abs_fulldata <- merge(pvalue_long, adjusted_r2_df)

write.csv(abs_fulldata, "cleaneddata/regress/abs_fulldata2.csv")

# Extract coefficients for each regression
coefficients <- lapply(regressions, function(model) {
  coef(model)  # Extract coefficients
})

coefficient_df <- do.call(rbind, coefficients)
colnames(coefficient_df) <- c("(Intercept)", "income_weekly", "hh_size", "rural", 
                              "Appalachia", "Delta", "GL_Midwest", "Heartland", "RemoteAreas",
                              "Northeast", "Northwest", "RioGrande", "Southeast", "Southwest",
                              "locally_grown", "organic", "local_econ", "afford",
                              "healthy", "social_resp", "access")
coefficient_df <- as.data.frame(coefficient_df)

# Add a column for the regression number
coefficient_df$Regression <- paste0("Regression_", 1:length(regressions))

# Arrange the data into a long format where each coefficient is its own column
coeffcient_long <- coefficient_df %>%
  select(Regression, "(Intercept)", "income_weekly", "hh_size", "rural", 
         "Appalachia", "Delta", "GL_Midwest", "Heartland", "RemoteAreas",
         "Northeast", "Northwest", "RioGrande", "Southeast", "Southwest",
         "locally_grown", "organic", "local_econ", "afford",
         "healthy", "social_resp", "access") %>%
  arrange(Regression)

write.csv(coeffcient_long, "cleaneddata/regress/cf_fulldata2.csv")


# regressions po_region -------------------------



regressions <- list(
  lm(supermarketwhole_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       po_region, data = fulldata),
  lm(supermarketfood_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       po_region, data = fulldata),
  lm(healthfood_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       po_region, data = fulldata),
  lm(convenience_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       po_region, data = fulldata),
  lm(online_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       po_region, data = fulldata),
  lm(discount_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       po_region, data = fulldata),
  lm(smallstore_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       po_region, data = fulldata),
  lm(farmmarket_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       po_region, data = fulldata),
  lm(directfarm_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       po_region, data = fulldata),
  lm(foodbox_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       po_region, data = fulldata),
  lm(mealkit_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       po_region, data = fulldata),
  lm(market_expend ~ income_weekly + hh_size + rural + 
       locally_grown + organic + local_econ + afford + healthy + social_resp + access + 
       po_region, data = fulldata) )



# Extract p-values for each regression
pvalues <- lapply(regressions, function(model) {
  summary(model)$coefficients[, "Pr(>|t|)"]
})


pvalue_df <- do.call(rbind, pvalues)
colnames(pvalue_df) <- c("(Intercept)", "income_weekly", "hh_size", "rural", 
                         "locally_grown", "organic", "local_econ", "afford",
                         "healthy", "social_resp", "access", "Northeast", "South", "West")
pvalue_df <- as.data.frame(pvalue_df)

# Add a column for the regression number
pvalue_df$Regression <- paste0("Regression_", 1:length(regressions))

# Arrange the data into a long format where each coefficient is its own column
pvalue_long <- pvalue_df %>%
  select(Regression, "(Intercept)", "income_weekly", "hh_size", "rural",
         "locally_grown", "organic", "local_econ", "afford",
         "healthy", "social_resp", "access", "Northeast", "South", "West") %>%
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

abs_fulldata <- merge(pvalue_long, adjusted_r2_df)

write.csv(abs_fulldata, "cleaneddata/regress/sig_poregion.csv")

# Extract coefficients for each regression
coefficients <- lapply(regressions, function(model) {
  coef(model)  # Extract coefficients
})

coefficient_df <- do.call(rbind, coefficients)
colnames(coefficient_df) <- c("(Intercept)", "income_weekly", "hh_size", "rural",
                              "locally_grown", "organic", "local_econ", "afford",
                              "healthy", "social_resp", "access", "Northeast", "South", "West")
coefficient_df <- as.data.frame(coefficient_df)

# Add a column for the regression number
coefficient_df$Regression <- paste0("Regression_", 1:length(regressions))

# Arrange the data into a long format where each coefficient is its own column
coeffcient_long <- coefficient_df %>%
  select(Regression, "(Intercept)", "income_weekly", "hh_size", "rural",
         "locally_grown", "organic", "local_econ", "afford",
         "healthy", "social_resp", "access", "Northeast", "South", "West") %>%
  arrange(Regression)

write.csv(coeffcient_long, "cleaneddata/regress/cf_poregion.csv")














# regressions region, no values ---------------------------------
  
  regressions <- list(
    lm(supermarketwhole_expend ~ income_weekly + hh_size + rural +
         Appalachia + Delta + GL_Midwest + Heartland + NorthCentral + Northeast +
         Northwest + RioGrande + Southeast + Southwest, data = fulldata),
    lm(supermarketfood_expend ~ income_weekly + hh_size + rural +
         Appalachia + Delta + GL_Midwest + Heartland + NorthCentral + Northeast +
         Northwest + RioGrande + Southeast + Southwest, data = fulldata),
    lm(healthfood_expend ~ income_weekly + hh_size + rural +
         Appalachia + Delta + GL_Midwest + Heartland + NorthCentral + Northeast +
         Northwest + RioGrande + Southeast + Southwest, data = fulldata),
    lm(convenience_expend ~ income_weekly + hh_size + rural +
         Appalachia + Delta + GL_Midwest + Heartland + NorthCentral + Northeast +
         Northwest + RioGrande + Southeast + Southwest, data = fulldata),
    lm(online_expend ~ income_weekly + hh_size + rural +
         Appalachia + Delta + GL_Midwest + Heartland + NorthCentral + Northeast +
         Northwest + RioGrande + Southeast + Southwest, data = fulldata),
    lm(discount_expend ~ income_weekly + hh_size + rural +
         Appalachia + Delta + GL_Midwest + Heartland + NorthCentral + Northeast +
         Northwest + RioGrande + Southeast + Southwest, data = fulldata),
    lm(smallstore_expend ~ income_weekly + hh_size + rural +
         Appalachia + Delta + GL_Midwest + Heartland + NorthCentral + Northeast +
         Northwest + RioGrande + Southeast + Southwest, data = fulldata),
    lm(farmmarket_expend ~ income_weekly + hh_size + rural +
         Appalachia + Delta + GL_Midwest + Heartland + NorthCentral + Northeast +
         Northwest + RioGrande + Southeast + Southwest, data = fulldata),
    lm(directfarm_expend ~ income_weekly + hh_size + rural +
         Appalachia + Delta + GL_Midwest + Heartland + NorthCentral + Northeast +
         Northwest + RioGrande + Southeast + Southwest, data = fulldata),
    lm(foodbox_expend ~ income_weekly + hh_size + rural +
         Appalachia + Delta + GL_Midwest + Heartland + NorthCentral + Northeast +
         Northwest + RioGrande + Southeast + Southwest, data = fulldata),
    lm(mealkit_expend ~ income_weekly + hh_size + rural +
         Appalachia + Delta + GL_Midwest + Heartland + NorthCentral + Northeast +
         Northwest + RioGrande + Southeast + Southwest, data = fulldata),
    lm(market_expend ~ income_weekly + hh_size + rural +
         Appalachia + Delta + GL_Midwest + Heartland + NorthCentral + Northeast +
         Northwest + RioGrande + Southeast + Southwest, data = fulldata) )



# Extract p-values for each regression
pvalues <- lapply(regressions, function(model) {
  summary(model)$coefficients[, "Pr(>|t|)"]
})


pvalue_df <- do.call(rbind, pvalues)
colnames(pvalue_df) <- c("(Intercept)", "income_weekly", "hh_size", "rural", 
                         "Appalachia", "Delta", "GL_Midwest", "Heartland", "NorthCentral",
                         "Northeast", "Northwest", "RioGrande", "Southeast", "Southwest")
pvalue_df <- as.data.frame(pvalue_df)

# Add a column for the regression number
pvalue_df$Regression <- paste0("Regression_", 1:length(regressions))

# Arrange the data into a long format where each coefficient is its own column
pvalue_long <- pvalue_df %>%
  select(Regression, "(Intercept)", "income_weekly", "hh_size", "rural", 
         "Appalachia", "Delta", "GL_Midwest", "Heartland", "NorthCentral",
         "Northeast", "Northwest", "RioGrande", "Southeast", "Southwest") %>%
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

abs_fulldata <- merge(pvalue_long, adjusted_r2_df)

write.csv(abs_fulldata, "cleaneddata/regress/novalue_sig.csv")

# Extract coefficients for each regression
coefficients <- lapply(regressions, function(model) {
  coef(model)  # Extract coefficients
})

coefficient_df <- do.call(rbind, coefficients)
colnames(coefficient_df) <- c("(Intercept)", "income_weekly", "hh_size", "rural", 
                              "Appalachia", "Delta", "GL_Midwest", "Heartland", "NorthCentral",
                              "Northeast", "Northwest", "RioGrande", "Southeast", "Southwest")
coefficient_df <- as.data.frame(coefficient_df)

# Add a column for the regression number
coefficient_df$Regression <- paste0("Regression_", 1:length(regressions))

# Arrange the data into a long format where each coefficient is its own column
coeffcient_long <- coefficient_df %>%
  select(Regression, "(Intercept)", "income_weekly", "hh_size", "rural", 
         "Appalachia", "Delta", "GL_Midwest", "Heartland", "NorthCentral",
         "Northeast", "Northwest", "RioGrande", "Southeast", "Southwest") %>%
  arrange(Regression)

write.csv(coeffcient_long, "cleaneddata/regress/novalue_cf.csv")


