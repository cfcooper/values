
## zinb model ##

library(tidyr) 
library(dplyr)
library(ggplot2)
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
noremote_full <- fulldata[!fulldata$RemoteAreas == 1, ]
remotearea <- fulldata[!fulldata$RemoteAreas == 0, ]

corrmatrix <- subset(noremote_full, select = c(rural,income_weekly,hh_size))
cor(corrmatrix)
summary(noremote_full$income_adj)

noremote_full$Q5 <- as.factor(noremote_full$Q5)
summary(noremote_full$Q5)



# expend tables -----------

fulldata$Q5 <- as.factor(fulldata$Q5)

df_wide_sum <- fulldata %>% group_by(Q5) %>%
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

noremote_full$Q8 <- as.factor(noremote_full$Q8)
noremote_full$Q8 <- relevel(factor(noremote_full$Q8), "12")
count_income <- noremote_full %>% group_by(Q8) %>%
  summarise(count = n())


region_summary <- noremote_full %>% group_by(region) %>%
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

write.csv(region_summary, "regionsummary.csv")



noremote_full <- noremote_full %>% mutate(
  ch1_zero = supermarketwhole_expend == 0,
  ch2_zero = supermarketfood_expend == 0,
  ch3_zero = healthfood_expend == 0,
  ch4_zero = convenience_expend == 0,
  ch5_zero = online_expend == 0,
  ch6_zero = discount_expend == 0,
  ch7_zero = smallstore_expend == 0,
  ch8_zero = farmmarket_expend == 0,
  ch9_zero = directfarm_expend == 0,
  ch10_zero = foodbox_expend == 0,
  ch11_zero = mealkit_expend == 0,
  ch12_zero = market_expend == 0) 

percent_values <- noremote_full %>%
  summarise(count_1 = sum(locally_grown == 1),
            count_2 = sum(organic == 1),
            count_3 = sum(local_econ == 1),
            count_4 = sum(afford == 1),
            count_5 = sum(healthy == 1),
            count_6 = sum(social_resp == 1),
            count_7 = sum(access == 1))

#noremote_full$locally_grown <- as.numeric(noremote_full$locally_grown)
#noremote_full$organic <- as.numeric(noremote_full$organic)
#noremote_full$local_econ <- as.numeric(noremote_full$local_econ)
#noremote_full$afford <- as.numeric(noremote_full$afford)
#noremote_full$healthy <- as.numeric(noremote_full$healthy)
#noremote_full$social_resp <- as.numeric(noremote_full$social_resp)
#noremote_full$access <- as.numeric(noremote_full$access)

count_values1 <- noremote_full %>% filter(ch1_zero == FALSE) %>%
  summarise(count_1 = sum(locally_grown == 1),
          count_2 = sum(organic == 1),
          count_3 = sum(local_econ == 1),
          count_4 = sum(afford == 1),
          count_5 = sum(healthy == 1),
          count_6 = sum(social_resp == 1),
          count_7 = sum(access == 1)) %>% mutate(sum_vars = rowSums(across(everything())))
count_values1$chan <- "1"

count_values2 <- noremote_full %>% filter(ch2_zero == FALSE) %>%
  summarise(count_1 = sum(locally_grown == 1),
            count_2 = sum(organic == 1),
            count_3 = sum(local_econ == 1),
            count_4 = sum(afford == 1),
            count_5 = sum(healthy == 1),
            count_6 = sum(social_resp == 1),
            count_7 = sum(access == 1)) %>% mutate(sum_vars = rowSums(across(everything())))
count_values2$chan <- "2"

count_values3 <- noremote_full %>% filter(ch3_zero == FALSE) %>%
  summarise(count_1 = sum(locally_grown == 1),
            count_2 = sum(organic == 1),
            count_3 = sum(local_econ == 1),
            count_4 = sum(afford == 1),
            count_5 = sum(healthy == 1),
            count_6 = sum(social_resp == 1),
            count_7 = sum(access == 1)) %>% mutate(sum_vars = rowSums(across(everything())))
count_values3$chan <- "3"

count_values4 <- noremote_full %>% filter(ch4_zero == FALSE) %>%
  summarise(count_1 = sum(locally_grown == 1),
            count_2 = sum(organic == 1),
            count_3 = sum(local_econ == 1),
            count_4 = sum(afford == 1),
            count_5 = sum(healthy == 1),
            count_6 = sum(social_resp == 1),
            count_7 = sum(access == 1)) %>% mutate(sum_vars = rowSums(across(everything())))
count_values4$chan <- "4"

count_values5 <- noremote_full %>% filter(ch5_zero == FALSE) %>%
  summarise(count_1 = sum(locally_grown == 1),
            count_2 = sum(organic == 1),
            count_3 = sum(local_econ == 1),
            count_4 = sum(afford == 1),
            count_5 = sum(healthy == 1),
            count_6 = sum(social_resp == 1),
            count_7 = sum(access == 1)) %>% mutate(sum_vars = rowSums(across(everything())))
count_values5$chan <- "5"

count_values6 <- noremote_full %>% filter(ch6_zero == FALSE) %>%
  summarise(count_1 = sum(locally_grown == 1),
            count_2 = sum(organic == 1),
            count_3 = sum(local_econ == 1),
            count_4 = sum(afford == 1),
            count_5 = sum(healthy == 1),
            count_6 = sum(social_resp == 1),
            count_7 = sum(access == 1)) %>% mutate(sum_vars = rowSums(across(everything())))
count_values6$chan <- "6"


count_values7 <- noremote_full %>% filter(ch7_zero == FALSE) %>%
  summarise(count_1 = sum(locally_grown == 1),
            count_2 = sum(organic == 1),
            count_3 = sum(local_econ == 1),
            count_4 = sum(afford == 1),
            count_5 = sum(healthy == 1),
            count_6 = sum(social_resp == 1),
            count_7 = sum(access == 1)) %>% mutate(sum_vars = rowSums(across(everything()))) 
count_values7$chan <- "7"

count_values8 <- noremote_full %>% filter(ch8_zero == FALSE) %>%
  summarise(count_1 = sum(locally_grown == 1),
            count_2 = sum(organic == 1),
            count_3 = sum(local_econ == 1),
            count_4 = sum(afford == 1),
            count_5 = sum(healthy == 1),
            count_6 = sum(social_resp == 1),
            count_7 = sum(access == 1)) %>% mutate(sum_vars = rowSums(across(everything()))) 
count_values8$chan <- "8"

count_values9 <- noremote_full %>% filter(ch9_zero == FALSE) %>%
  summarise(count_1 = sum(locally_grown == 1),
            count_2 = sum(organic == 1),
            count_3 = sum(local_econ == 1),
            count_4 = sum(afford == 1),
            count_5 = sum(healthy == 1),
            count_6 = sum(social_resp == 1),
            count_7 = sum(access == 1)) %>% mutate(sum_vars = rowSums(across(everything()))) 
count_values9$chan <- "9"

count_values10 <- noremote_full %>% filter(ch10_zero == FALSE) %>%
  summarise(count_1 = sum(locally_grown == 1),
            count_2 = sum(organic == 1),
            count_3 = sum(local_econ == 1),
            count_4 = sum(afford == 1),
            count_5 = sum(healthy == 1),
            count_6 = sum(social_resp == 1),
            count_7 = sum(access == 1)) %>% mutate(sum_vars = rowSums(across(everything()))) 
count_values10$chan <- "10"

count_values11 <- noremote_full %>% filter(ch11_zero == FALSE) %>%
  summarise(count_1 = sum(locally_grown == 1),
            count_2 = sum(organic == 1),
            count_3 = sum(local_econ == 1),
            count_4 = sum(afford == 1),
            count_5 = sum(healthy == 1),
            count_6 = sum(social_resp == 1),
            count_7 = sum(access == 1)) %>% mutate(sum_vars = rowSums(across(everything()))) 
count_values11$chan <- "11"

count_values12 <- noremote_full %>% filter(ch12_zero == FALSE) %>%
  summarise(count_1 = sum(locally_grown == 1),
            count_2 = sum(organic == 1),
            count_3 = sum(local_econ == 1),
            count_4 = sum(afford == 1),
            count_5 = sum(healthy == 1),
            count_6 = sum(social_resp == 1),
            count_7 = sum(access == 1)) %>% mutate(sum_vars = rowSums(across(everything()))) 
count_values12$chan <- "12"

fullvalue_count <- rbind(count_values1, count_values2, count_values3, count_values4, 
                         count_values5, count_values6, count_values7, count_values8,
                         count_values9, count_values10, count_values11, count_values12)

fullvalue_count <- fullvalue_count %>%
  mutate(across(starts_with("count_"), ~ .x / sum_vars)*100)

fullvalue_count <- fullvalue_count %>% rename(locally_grown = count_1)
fullvalue_count <- fullvalue_count %>% rename(organic = count_2)
fullvalue_count <- fullvalue_count %>% rename(local_econ = count_3)
fullvalue_count <- fullvalue_count %>% rename(afford = count_4)
fullvalue_count <- fullvalue_count %>% rename(healthy = count_5)
fullvalue_count <- fullvalue_count %>% rename(social_resp = count_6)
fullvalue_count <- fullvalue_count %>% rename(access = count_7)

# Reshape to long format
long_data <- pivot_longer(
  fullvalue_count,
  cols = c(locally_grown, organic, local_econ, afford,
           healthy, social_resp, access),
  names_to = "value",
  values_to = "percent"
)




long_data <- long_data %>% mutate(value = factor(value, levels=c("social_resp", "locally_grown", "local_econ", 
                                                                 "organic", "access", "healthy", "afford")))

long_data$chan <- if_else(long_data$chan == 1, "Supercenter and wholesale", long_data$chan)
long_data$chan <- if_else(long_data$chan == 2, "Supermarket and grocery", long_data$chan)
long_data$chan <- if_else(long_data$chan == 3, "Health/natural supermarket", long_data$chan)
long_data$chan <- if_else(long_data$chan == 4, "Convenience store/corner store", long_data$chan)
long_data$chan <- if_else(long_data$chan == 5, "Online-only retailer", long_data$chan)
long_data$chan <- if_else(long_data$chan == 6, "Discount store", long_data$chan)
long_data$chan <- if_else(long_data$chan == 7, "Smaller format grocery store", long_data$chan)
long_data$chan <- if_else(long_data$chan == 8, "Farmers Market", long_data$chan)
long_data$chan <- if_else(long_data$chan == 9, "Direct from producer", long_data$chan)
long_data$chan <- if_else(long_data$chan == 10, "Food Box", long_data$chan)
long_data$chan <- if_else(long_data$chan == 11, "Meal/Meal Kit Delivery Service", long_data$chan)
long_data$chan <- if_else(long_data$chan == 12, "Bakery, deli, meat or fish market", long_data$chan)


table_v <- ggplot(data = long_data, aes(x = chan, y= percent, fill = value)) +  geom_col(width = .5) + theme_minimal()
table_v



write.csv(fullvalue_count, "valuecount_table.csv")


# models 1 ---------------------------------------------


m1 <- zeroinfl(supermarketwhole_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 Q8 + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m1)



m2 <- zeroinfl(supermarketfood_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 Q8 + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m2)


m3 <- zeroinfl(healthfood_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 Q8 + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m3)


m4 <- zeroinfl(convenience_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 Q8 + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m4)


m5 <- zeroinfl(online_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 Q8 + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m5)

m6 <- zeroinfl(discount_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 Q8 + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m6)


m7 <- zeroinfl(smallstore_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 Q8 + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m7)


m8 <- zeroinfl(farmmarket_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 Q8 + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m8)


m9 <- zeroinfl(directfarm_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 Q8 + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m9)

m10 <- zeroinfl(foodbox_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                  Q8 + rural + Q5 + hh_size |
                  Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                  Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m10)

m11 <- zeroinfl(mealkit_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                  Q8 + rural + Q5 + hh_size |
                  Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                  Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m11)

m12 <- zeroinfl(market_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                  Q8 + rural + Q5 + hh_size |
                  Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                  Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
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
                 Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m1)

m2 <- zeroinfl(supermarketfood_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 + hh_size  |
                 Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m2)


m3 <- zeroinfl(healthfood_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m3)


m4 <- zeroinfl(convenience_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m4)


m5 <- zeroinfl(online_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m5)

m6 <- zeroinfl(discount_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m6)


m7 <- zeroinfl(smallstore_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m7)


m8 <- zeroinfl(farmmarket_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m8)


m9 <- zeroinfl(directfarm_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                 income_weekly + rural + Q5 + hh_size |
                 Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                 Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m9)

m10 <- zeroinfl(foodbox_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                  income_weekly + rural + Q5 + hh_size |
                  Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                  Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m10)

m11 <- zeroinfl(mealkit_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                  income_weekly + rural + Q5 + hh_size |
                  Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                  Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
summary(m11)

m12 <- zeroinfl(market_expend ~ afford + healthy + access + locally_grown + local_econ + social_resp + organic + 
                  income_weekly + rural + Q5 + hh_size |
                  Appalachia + Delta + GL_Midwest + Heartland + NorthCentral +
                  Northwest + RioGrande + Southeast + Southwest + rural, data = noremote_full, dist = "negbin")
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


age <- noremote_full %>% group_by(Q5) %>%
  summarise(expend = mean(sum_expend))

remotearea <- noremote_full[!noremote_full$RemoteAreas == 0, ]

# zeros vs non zeros ------------------------------------

cols <- 52:65  # the expend columns
expend_data <- noremote_full[, cols]

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





