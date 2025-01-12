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
nondelta <- fulldat[!fulldat$region_Delta == 1, ]                                # subset: non-delta 
factors <- subset(fulldat, select = c(4,9,164,173))                              # subset: value factors 


factor_long <- factors %>% separate_rows(fv_top_three_factors, sep = ",")  %>%   # make long data for value factors
  mutate(fv_top_three_factors = as.numeric(fv_top_three_factors))

df_wide <- factor_long %>%
  mutate(value = 1) %>%  # Add a column for binary indicator (1 for presence)
  pivot_wider(names_from = fv_top_three_factors, values_from = value, 
              values_fill = 0, names_glue = "value_{fv_top_three_factors}")      # make wide data for value factors 

names(df_wide)[names(df_wide) == "value_1"] <- "locally_grown"
names(df_wide)[names(df_wide) == "value_2"] <- "organic"
names(df_wide)[names(df_wide) == "value_3"] <- "local_econ"
names(df_wide)[names(df_wide) == "value_4"] <- "afford"
names(df_wide)[names(df_wide) == "value_5"] <- "healthy"
names(df_wide)[names(df_wide) == "value_6"] <- "social_resp"
names(df_wide)[names(df_wide) == "value_7"] <- "access"

delta_factors <- df_wide[!df_wide$region_Delta == 0, ]
nondelta_factors <- df_wide[!df_wide$region_Delta == 1, ]

df_wide$region_Delta <- as.factor(df_wide$region_Delta)

df_wide_sum <- df_wide %>% group_by(region_Delta) %>%
  summarise(count_1 = sum(locally_grown),
            count_2 = sum(organic),
            count_3 = sum(local_econ),
            count_4 = sum(afford),
            count_5 = sum(healthy),
            count_6 = sum(social_resp),
            count_7 = sum(access))

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
names(delta)[names(delta) == "Q20.1_10_1"] <- "foodbox_expend"
names(delta)[names(delta) == "Q20.1_11_1"] <- "mealkit_expend"
names(delta)[names(delta) == "Q20.1_12_1"] <- "market_expend"
names(delta)[names(delta) == "Q20.1_13_1"] <- "chainrest_expend"
names(delta)[names(delta) == "Q20.1_14_1"] <- "localrest_expend"

names(delta)[names(delta) == "Q24.w.attn.check_1"] <- "PCE_health"
names(delta)[names(delta) == "Q24.w.attn.check_2"] <- "PCE_local"
names(delta)[names(delta) == "Q24.w.attn.check_3"] <- "PCE_socialresp"
names(delta)[names(delta) == "Q24.w.attn.check_6"] <- "PCE_gender"

## convert values ----------------------------------------------------------



## convert income ----------------------------------------------------------

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

delta[, 52:65][is.na(delta[, 52:65])] <- 0

delta <- delta %>%
  rowwise() %>%
  mutate(sum_expend = sum(across(ends_with("expend")), na.rm = T))
delta <- delta[-which(delta$sum_expend <= 0),]

delta <- delta %>%
  rowwise() %>%
  mutate(sumPCE = sum(across(starts_with("Q24.w")), na.rm = T))


sm_whole_expend <- mean(delta$supermarketwhole_expend)+ sd(delta$supermarketwhole_expend)*2
sm_food_expend <- mean(delta$supermarketfood_expend)+ sd(delta$supermarketfood_expend)*2
conv_expend <- mean(delta$convenience_expend)+ sd(delta$convenience_expend)*2


delta <- delta[-which(delta$supermarketwhole_expend > sm_whole_expend),]
delta <- delta[-which(delta$supermarketfood_expend > sm_food_expend),]
delta <- delta[-which(delta$convenience_expend > conv_expend),]


delta$supermarketwhole_expend_p <- delta$supermarketwhole_expend/(delta$income/52)
delta$supermarketfood_expend_p <- delta$supermarketfood_expend/(delta$income/52)
delta$healthfood_expend_p <- delta$healthfood_expend/(delta$income/52)
delta$convenience_expend_p <- delta$convenience_expend/(delta$income/52)
delta$online_expend_p <- delta$online_expend/(delta$income/52)
delta$discount_expend_p <- delta$discount_expend/(delta$income/52)
delta$smallstore_expend_p <- delta$smallstore_expend/(delta$income/52)
delta$farmmarket_expend_p <- delta$farmmarket_expend/(delta$income/52)
delta$directfarm_expend_p <- delta$directfarm_expend/(delta$income/52)


col_names <- colnames(delta)
col_positions <- seq_along(col_names)
col_info <- data.frame(Column = col_names, Position = col_positions)             # to make col reference dataframe

delta$supermarketwhole_expend_t <- delta$supermarketwhole_expend/delta$sum_expend
delta$supermarketfood_expend_t <- delta$supermarketfood_expend/delta$sum_expend
delta$healthfood_expend_t <- delta$healthfood_expend/delta$sum_expend
delta$convenience_expend_t <- delta$convenience_expend/delta$sum_expend
delta$online_expend_t <- delta$online_expend/delta$sum_expend
delta$discount_expend_t <- delta$discount_expend/delta$sum_expend
delta$smallstore_expend_t <- delta$smallstore_expend/delta$sum_expend
delta$farmmarket_expend_t <- delta$farmmarket_expend/delta$sum_expend
delta$directfarm_expend_t <- delta$directfarm_expend/delta$sum_expend


#supermarketwhole <- delta[ c(4,186) ]
#delta_expend <- delta[ c(4,52:60) ]

delta$rural <- if_else(delta$Q101 == 1, 1, 0)
delta$urban <- if_else(delta$Q101 == 3, 1, 0)
delta$suburban <- if_else(delta$Q101 == 2, 1, 0)

## finish non delta -----------------------------------------------------------

nondelta$foodsecure <- ifelse(nondelta$Q42 == 2,0,1)

names(nondelta)[names(nondelta) == "Q20.1_1_1"] <- "supermarketwhole_expend"
names(nondelta)[names(nondelta) == "Q20.1_2_1"] <- "supermarketfood_expend"
names(nondelta)[names(nondelta) == "Q20.1_3_1"] <- "healthfood_expend"
names(nondelta)[names(nondelta) == "Q20.1_4_1"] <- "convenience_expend"
names(nondelta)[names(nondelta) == "Q20.1_5_1"] <- "online_expend"
names(nondelta)[names(nondelta) == "Q20.1_6_1"] <- "discount_expend"
names(nondelta)[names(nondelta) == "Q20.1_7_1"] <- "smallstore_expend"
names(nondelta)[names(nondelta) == "Q20.1_8_1"] <- "farmmarket_expend"
names(nondelta)[names(nondelta) == "Q20.1_9_1"] <- "directfarm_expend"
names(nondelta)[names(nondelta) == "Q20.1_10_1"] <- "foodbox_expend"
names(nondelta)[names(nondelta) == "Q20.1_11_1"] <- "mealkit_expend"
names(nondelta)[names(nondelta) == "Q20.1_12_1"] <- "market_expend"
names(nondelta)[names(nondelta) == "Q20.1_13_1"] <- "chainrest_expend"
names(nondelta)[names(nondelta) == "Q20.1_14_1"] <- "localrest_expend"

names(nondelta)[names(nondelta) == "Q24.w.attn.check_1"] <- "PCE_health"
names(nondelta)[names(nondelta) == "Q24.w.attn.check_2"] <- "PCE_local"
names(nondelta)[names(nondelta) == "Q24.w.attn.check_3"] <- "PCE_socialresp"
names(nondelta)[names(nondelta) == "Q24.w.attn.check_6"] <- "PCE_gender"

nondelta <- nondelta %>%
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

nondelta[, 52:65][is.na(nondelta[, 52:65])] <- 0

nondelta <- nondelta %>%
  rowwise() %>%
  mutate(sum_expend = sum(across(ends_with("expend")), na.rm = T))
nondelta <- nondelta[-which(nondelta$sum_expend <= 0),]

nondelta <- nondelta %>%
  rowwise() %>%
  mutate(sumPCE = sum(across(starts_with("Q24.w")), na.rm = T))


sm_whole_expend <- mean(nondelta$supermarketwhole_expend)+ sd(nondelta$supermarketwhole_expend)*2
sm_food_expend <- mean(nondelta$supermarketfood_expend)+ sd(nondelta$supermarketfood_expend)*2
conv_expend <- mean(nondelta$convenience_expend)+ sd(nondelta$convenience_expend)*2


nondelta <- nondelta[-which(nondelta$supermarketwhole_expend > sm_whole_expend),]
nondelta <- nondelta[-which(nondelta$supermarketfood_expend > sm_food_expend),]
nondelta <- nondelta[-which(nondelta$convenience_expend > conv_expend),]


nondelta$supermarketwhole_expend_p <- nondelta$supermarketwhole_expend/(nondelta$income/52)
nondelta$supermarketfood_expend_p <- nondelta$supermarketfood_expend/(nondelta$income/52)
nondelta$healthfood_expend_p <- nondelta$healthfood_expend/(nondelta$income/52)
nondelta$convenience_expend_p <- nondelta$convenience_expend/(nondelta$income/52)
nondelta$online_expend_p <- nondelta$online_expend/(nondelta$income/52)
nondelta$discount_expend_p <- nondelta$discount_expend/(nondelta$income/52)
nondelta$smallstore_expend_p <- nondelta$smallstore_expend/(nondelta$income/52)
nondelta$farmmarket_expend_p <- nondelta$farmmarket_expend/(nondelta$income/52)
nondelta$directfarm_expend_p <- nondelta$directfarm_expend/(nondelta$income/52)


col_names <- colnames(nondelta)
col_positions <- seq_along(col_names)
col_info <- data.frame(Column = col_names, Position = col_positions)             # to make col reference dataframe

nondelta$supermarketwhole_expend_t <- nondelta$supermarketwhole_expend/nondelta$sum_expend
nondelta$supermarketfood_expend_t <- nondelta$supermarketfood_expend/nondelta$sum_expend
nondelta$healthfood_expend_t <- nondelta$healthfood_expend/nondelta$sum_expend
nondelta$convenience_expend_t <- nondelta$convenience_expend/nondelta$sum_expend
nondelta$online_expend_t <- nondelta$online_expend/nondelta$sum_expend
nondelta$discount_expend_t <- nondelta$discount_expend/nondelta$sum_expend
nondelta$smallstore_expend_t <- nondelta$smallstore_expend/nondelta$sum_expend
nondelta$farmmarket_expend_t <- nondelta$farmmarket_expend/nondelta$sum_expend
nondelta$directfarm_expend_t <- nondelta$directfarm_expend/nondelta$sum_expend



nondelta$rural <- if_else(nondelta$Q101 == 1, 1, 0)
nondelta$urban <- if_else(nondelta$Q101 == 3, 1, 0)
nondelta$suburban <- if_else(nondelta$Q101 == 2, 1, 0)

nondelta_factors <- nondelta_factors[ -c(2:3) ]
nondelta <- merge(nondelta, nondelta_factors, by = "responseID")



summary_nondelta <- nondelta %>%
  select(ends_with("_expend")) %>%  # Select columns ending with "_expend"
  summarise(
    across(
      everything(), 
      list(mean = ~ mean(.), sd = ~ sd(.)), 
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("column", ".value"),
    names_sep = "_"
  )



## write rds files -------------------------------------------------------------

delta_factors <- delta_factors[ -c(2:3) ]
delta <- merge(delta, delta_factors, by = "responseID")


saveRDS(delta, "cleaneddata/delta.rds")
saveRDS(nondelta, "cleaneddata/nondelta.rds")







