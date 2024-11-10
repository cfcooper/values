
# regressions

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


delta$percent <- delta$supermarketwhole_expend/delta$income

names(delta)[names(delta) == "Q20.1_1_1"] <- "supermarketwhole_expend"


super_whole_reg <- lm(supermarketwhole_expend ~foodsecure + value_1 + value_2 + value_3 + value_4 +
                        value_5 + value_6 + value_7 + income, data = delta)
summary(super_whole_reg)
max(delta$supermarketwhole_expend)


super_health_reg <- lm(healthfood_expend ~foodsecure + value_1 + value_2 + value_3 + value_4 +
                         value_5 + value_6 + value_7 + income, data = delta)
summary(super_health_reg)


conv_reg <- lm(convenience_expend ~foodsecure + value_1 + value_2 + value_3 + value_4 +
                          value_5 + value_6 + value_7 + income, data = delta)
summary(conv_reg)


farmmark_reg <- lm(farmmarket_expend ~foodsecure + value_1 + value_2 + value_3 + value_4 +
                     value_5 + value_6 + value_7 + income, data = delta)
summary(farmmark_reg)


summary(delta)


