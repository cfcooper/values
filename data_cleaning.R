# data cleaning

library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(maps)
library(mapdata)
library(reshape)
library(magrittr)
library(formattable)


rm(list=ls()) # Caution: this clears the Environment

## subset dataframes -----------------------------------------------------------

fulldat <- read.csv("dataset_numeric.csv")

col_names <- colnames(fulldat)
col_positions <- seq_along(col_names)

# Combine column names and positions
col_info <- data.frame(Column = col_names, Position = col_positions)

names(fulldat)[names(fulldat) == "Q102"] <- "fv_top_three_factors"

fulldat$comma <- str_count(fulldat$fv_top_three_factors, ",")
fulldat <- fulldat[!fulldat$comma > 2, ]

fulldat$region_Delta <- ifelse(fulldat$Q1 %in% c(4,18,19,25,44),1,0)

summary(fulldat)

demographics <- subset(fulldat, select = c(4,9:15,165:170,173))
delta <- fulldat[!fulldat$region_Delta == 0, ]

factors <- subset(fulldat, select = c(4,149:164,173))

factor_long <- factors %>% separate_rows(fv_top_three_factors, sep = ",")  %>%
  mutate(fv_top_three_factors = as.numeric(fv_top_three_factors))

df_wide <- factor_long %>%
  mutate(value = 1) %>%  # Add a column for binary indicator (1 for presence)
  pivot_wider(names_from = fv_top_three_factors, values_from = value, values_fill = 0, names_glue = "value_{fv_top_three_factors}")  # Fill missing values with 0

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

df_wide_sum[1,2:8] <- df_wide_sum[1, ] / 4709
df_wide_sum[2,2:8] <- df_wide_sum[2, ] / 343
