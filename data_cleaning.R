# data cleaning

library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape)
library(magrittr)
library(formattable)


rm(list=ls()) # Caution: this clears the Environment

## subset dataframes -----------------------------------------------------------

fulldat <- read.csv("dataset_numeric.csv")


col_names <- colnames(fulldat)
col_positions <- seq_along(col_names)
col_info <- data.frame(Column = col_names, Position = col_positions)             # to make col reference dataframe


names(fulldat)[names(fulldat) == "Q102"] <- "fv_top_three_factors"
fulldat$comma <- str_count(fulldat$fv_top_three_factors, ",")
fulldat <- fulldat[!fulldat$comma > 2, ]                                         # remove if more than 3 in top 3 factors


fulldat$region_Delta <- ifelse(fulldat$Q1 %in% c(4,18,19,25,44),1,0)             # delta variable


demographics <- subset(fulldat, select = c(4,9:15,165:170,173))                  # subset: demographics 
delta <- fulldat[!fulldat$region_Delta == 0, ]                                   # subset: delta 
factors <- subset(fulldat, select = c(4,9,164,173))                              # subset: value factors 


factor_long <- factors %>% separate_rows(fv_top_three_factors, sep = ",")  %>%   # make long data for value factors
  mutate(fv_top_three_factors = as.numeric(fv_top_three_factors))

df_wide <- factor_long %>%
  mutate(value = 1) %>%  # Add a column for binary indicator (1 for presence)
  pivot_wider(names_from = fv_top_three_factors, values_from = value, 
              values_fill = 0, names_glue = "value_{fv_top_three_factors}")      # make wide data for value factors 

delta_factors <- df_wide[!df_wide$region_Delta == 0, ]
df_wide$region_Delta <- as.factor(df_wide$region_Delta)

df_wide_sum <- df_wide %>% group_by(region_Delta) %>%
  summarise(count_1 = sum(value_1),
            count_2 = sum(value_2),
            count_3 = sum(value_3),
            count_4 = sum(value_4),
            count_5 = sum(value_5),
            count_6 = sum(value_6),
            count_7 = sum(value_7))

df_wide_sum[1,2:8] <- df_wide_sum[1,2:8] / 4709                                  # convert obs into % of delta/nondelta to choose factor
df_wide_sum[2,2:8] <- df_wide_sum[2,2:8] / 343

## make other variables ---------------------------------------------------------

delta$foodsecure <- ifelse(delta$Q42 == 2,0,1)

#delta <- delta %>%
#  mutate(income = case_when(
#    Q9 >= 1 & Q9 <= 3 ~ 1,
#    Q9 >= 4 & Q9 <= 6 ~ 2,
#    Q9 >= 7 & Q9 <= 9 ~ 3,
#    Q9 >= 10 & Q9 <= 12 ~ 4,
#    TRUE ~ NA_real_))

names(delta)[names(delta) == "Q20.1_1_1"] <- "supermarketwhole_expend"
names(delta)[names(delta) == "Q20.1_2_1"] <- "supermarketfood_expend"
names(delta)[names(delta) == "Q20.1_3_1"] <- "healthfood_expend"
names(delta)[names(delta) == "Q20.1_4_1"] <- "convenience_expend"
names(delta)[names(delta) == "Q20.1_5_1"] <- "online_expend"
names(delta)[names(delta) == "Q20.1_6_1"] <- "discount_expend"
names(delta)[names(delta) == "Q20.1_7_1"] <- "smallstore_expend"
names(delta)[names(delta) == "Q20.1_8_1"] <- "farmmarket_expend"
names(delta)[names(delta) == "Q20.1_9_1"] <- "directfarm_expend"


## convert values ----------------------------------------------------------



## convert income ----------------------------------------------------------
#delta$Q9 <- as.logical(delta$Q9)
delta <- delta %>%
  mutate(income = case_when(
    Q9 == 1 ~ 10000,
    Q9 == 2 ~ 15000,
    Q9 == 3 ~ 25000,
    Q9 == 4 ~ 35000,
    Q9 == 5 ~ 45000,
    Q9 == 6 ~ 55000,
    Q9 == 7 ~ 65000,
    Q9 == 8 ~ 75000,
    Q9 == 9 ~ 85000,
    Q9 == 10 ~ 95000,
    Q9 == 11 ~ 125000,
    Q9 == 12 ~ 150000,
    TRUE ~ NA_real_))

delat$supermarketwhole_expend_p <- 

## write rds files -------------------------------------------------------------

delta_factors <- delta_factors[ -c(2:3) ]
delta <- merge(delta, delta_factors, by = "responseID")


saveRDS(delta, "cleaneddata/delta.rds")








