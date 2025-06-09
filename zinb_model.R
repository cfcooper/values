
## zinb model ##

library(tidyr) 
library(dplyr)
#library(ggplot2)
#library(reshape)
library(magrittr)
#library(formattable)
library(MASS)
library(pscl)
library(boot)



rm(list=ls()) # caution: this clears the environment

## read in dataframes -----------------------------------------------------------

fulldata <- readRDS("fulldata.rds")
fulldata <- fulldata[!is.na(fulldata$hh_size), ]
fulldata <- fulldata[!is.na(fulldata$rural), ]

fulldata$market_expend <- as.integer(fulldata$market_expend)
fulldata$Q5 <- as.numeric(fulldata$Q5)


corrmatrix <- subset(fulldata, select = c(rural,income_weekly,hh_size,Q5))
cor(corrmatrix)
summary(fulldata$income_adj)

fulldata$Q5 <- as.factor(fulldata$Q5)
summary(fulldata$Q5)

# models 1 ---------------------------------------------


m1 <- zeroinfl(supermarketwhole_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 |
                 Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m1)

m2 <- zeroinfl(supermarketfood_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 |
                 Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m2)


m3 <- zeroinfl(healthfood_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 |
                 Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m3)


m4 <- zeroinfl(convenience_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 |
                 Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m4)


m5 <- zeroinfl(online_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 |
                 Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m5)

m6 <- zeroinfl(discount_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 |
                 Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m6)


m7 <- zeroinfl(smallstore_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 |
                 Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m7)


m8 <- zeroinfl(farmmarket_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 |
                 Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m8)


m9 <- zeroinfl(directfarm_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 |
                 Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m9)

m10 <- zeroinfl(foodbox_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 |
                 Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m10)

m11 <- zeroinfl(mealkit_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                  income_weekly + rural + Q5 |
                  Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                  Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m11)

m12 <- zeroinfl(market_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                  income_weekly + rural + Q5 |
                  Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                  Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m12)

for (i in 1:12) {
  model <- get(paste0("m", i))  # Access model by name (e.g., m1, m2, ..., m12)
  
  # Extract coefficients
  coefficients_count <- summary(model)$coefficients$count[, "Estimate"]
  coefficients_zero <- summary(model)$coefficients$zero[, "Estimate"]
  
  # Create data frames
  df_count <- data.frame(variable = names(coefficients_count), count_coef = coefficients_count)
  df_zero <- data.frame(variable = names(coefficients_zero), zero_coef = coefficients_zero)
  
  # Save to CSV files
  write.csv(df_count, file = paste0("cleaneddata/zinb_model_", i, "_count.csv"), row.names = FALSE)
  write.csv(df_zero, file = paste0("cleaneddata/zinb_model_", i, "_zero.csv"), row.names = FALSE)
}



# models 2 ---------------------------------------------


m1 <- zeroinfl(supermarketwhole_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m1)

m2 <- zeroinfl(supermarketfood_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 + hh_size  |
                 Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m2)


m3 <- zeroinfl(healthfood_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m3)


m4 <- zeroinfl(convenience_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m4)


m5 <- zeroinfl(online_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m5)

m6 <- zeroinfl(discount_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m6)


m7 <- zeroinfl(smallstore_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m7)


m8 <- zeroinfl(farmmarket_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m8)


m9 <- zeroinfl(directfarm_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m9)

m10 <- zeroinfl(foodbox_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                  income_weekly + rural + Q5 + hh_size |
                  Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                  Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m10)

m11 <- zeroinfl(mealkit_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                  income_weekly + rural + Q5 + hh_size |
                  Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                  Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m11)

m12 <- zeroinfl(market_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                  income_weekly + rural + Q5 + hh_size |
                  Appalachia + Delta + GL_Midwest + Heartland + RemoteAreas + NorthCentral +
                  Northwest + RioGrande + Southeast + Southwest + rural, data = fulldata, dist = "negbin")
summary(m12)

for (i in 1:12) {
  model <- get(paste0("m", i))  # Access model by name (e.g., m1, m2, ..., m12)
  
  # Extract coefficients
  coefficients_count <- summary(model)$coefficients$count[, "Estimate"]
  coefficients_zero <- summary(model)$coefficients$zero[, "Estimate"]
  
  # Create data frames
  df_count <- data.frame(variable = names(coefficients_count), count_coef = coefficients_count)
  df_zero <- data.frame(variable = names(coefficients_zero), zero_coef = coefficients_zero)
  
  # Save to CSV files
  write.csv(df_count, file = paste0("cleaneddata/3zinb_model_", i, "_count.csv"), row.names = FALSE)
  write.csv(df_zero, file = paste0("cleaneddata/3zinb_model_", i, "_zero.csv"), row.names = FALSE)
}


age <- fulldata %>% group_by(Q5) %>%
  summarise(expend = mean(sum_expend))

remotearea <- fulldata[!fulldata$RemoteAreas == 0, ]

# zeros vs non zeros ------------------------------------

cols <- 52:65  # the expend columns
expend_data <- fulldata[, cols]

# Ensure all columns are numeric
expend_data <- data.frame(lapply(expend_data, as.numeric))

# Count zeros and non-zeros per column
zero_counts <- sapply(expend_data, function(x) sum(x == 0, na.rm = TRUE))
nonzero_counts <- sapply(expend_data, function(x) sum(x != 0, na.rm = TRUE))

# Combine into one data frame
counts_df <- data.frame(
  variable = names(zero_counts),
  zeros = zero_counts,
  nonzeros = nonzero_counts,
  row.names = NULL
)


write.csv(counts_df, "cleaneddata/zerocounts.csv")

# alphas 

alpha_m1 <- 1 / m1$theta
alpha_m2 <- 1 / m2$theta
alpha_m3 <- 1 / m3$theta
alpha_m4 <- 1 / m4$theta
alpha_m5 <- 1 / m5$theta
alpha_m6 <- 1 / m6$theta
alpha_m7 <- 1 / m7$theta
alpha_m8 <- 1 / m8$theta
alpha_m9 <- 1 / m9$theta
alpha_m10 <- 1 / m10$theta
alpha_m11 <- 1 / m11$theta

alpha_names <- ls(pattern = "^alpha_m")

# Create a data frame with model name and alpha value
alpha_df <- data.frame(
  model = gsub("alpha_", "", alpha_names),
  alpha = sapply(alpha_names, function(x) get(x))
)

# Sort by model name if needed
alpha_df <- alpha_df[order(alpha_df$model), ]
write.csv(alpha_df, "cleaneddata/alpha_df.csv")





