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
#fulldat <- fulldat[!fulldat$comma > 2, ]

fulldat$region_Delta <- ifelse(fulldat$Q1 %in% c(4,18,19,25,44),1,0)

summary(fulldat)

demographics <- subset(fulldat, select = c(4,9:15,165:170,173))
delta <- fulldat[!fulldat$region_Delta == 0, ]

factors <- subset(fulldat, select = c(4,149:164,173))

factors %<>% mutate(t2 = fv_top_three_factors) %>% separate_rows(fv_top_three_factors, sep = ",")


# Q20 (channel expend)
expenditure_channel <- delta[, c("Q20.1_1_1", "Q20.1_2_1","Q20.1_3_1",
                                 "Q20.1_4_1", "Q20.1_5_1","Q20.1_6_1",
                                 "Q20.1_7_1", "Q20.1_8_1","Q20.1_9_1",
                                 "Q20.1_10_1", "Q20.1_11_1","Q20.1_12_1",
                                 "Q20.1_13_1", "Q20.1_14_1")]

mean_sd <- function(x) {
  c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
}

result <- apply(expenditure_channel, 2, mean_sd)
result <- as.data.frame(result)
result$ID <- c("mean", "SD")

df_long <- result %>%
  pivot_longer(cols = -ID,   # Specify the columns to pivot (all columns in this case)
               names_to = "channel", # Name for the column that holds the former column names
               values_to = "value")   # Name for the column that holds the values

df_wider <- df_long %>%
  pivot_wider(names_from = ID, values_from = value)

write.csv(df_wider, "channel_expend.csv")

columns_to_replace <- c(52:65)
delta[, columns_to_replace] <- lapply(delta[, columns_to_replace], function(x) replace(x, is.na(x), 0))

delta$expend_calc <- rowSums(delta[, c(52:65)])










# Q12 (category expend)


expenditure_category <- delta[, c(16:21)]

result <- apply(expenditure_category, 2, mean_sd)
result <- as.data.frame(result)
result$ID <- c("mean", "SD")

df_long <- result %>%
  pivot_longer(cols = -ID,   # Specify the columns to pivot (all columns in this case)
               names_to = "channel", # Name for the column that holds the former column names
               values_to = "value")   # Name for the column that holds the values

df_category <- df_long %>%
  pivot_wider(names_from = ID, values_from = value)

write.csv(df_category, "category_expend.csv")



# Q21 (important as a factor - values)


value_ranking <- delta[, c(149:157)]

result <- apply(value_ranking, 2, mean_sd)
result <- as.data.frame(result)
result$ID <- c("mean", "SD")

df_long <- result %>%
  pivot_longer(cols = -ID,   # Specify the columns to pivot (all columns in this case)
               names_to = "category", # Name for the column that holds the former column names
               values_to = "value")   # Name for the column that holds the values

df_value <- df_long %>%
  pivot_wider(names_from = ID, values_from = value)

write.csv(df_value, "value_rank.csv")

# Q24 (PCE)


PCE_avg <- delta[, c(158:163)]

result <- apply(PCE_avg, 2, mean_sd)
result <- as.data.frame(result)
result$ID <- c("mean", "SD")

df_long <- result %>%
  pivot_longer(cols = -ID,   # Specify the columns to pivot (all columns in this case)
               names_to = "PCE", # Name for the column that holds the former column names
               values_to = "value")   # Name for the column that holds the values

df_pce <- df_long %>%
  pivot_wider(names_from = ID, values_from = value)

write.csv(df_pce, "pce_avg.csv")





