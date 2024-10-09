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

factors %<>% mutate(t2 = fv_top_three_factors) %>% separate_rows(fv_top_three_factors, sep = ",")



