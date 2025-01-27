
## summary stats


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

nondelta$income_weekly <- nondelta$income/52
delta$income_weekly <- delta$income/52

delta$rucc <- as.numeric(delta$rucc)
nondelta$rucc <- as.numeric(nondelta$rucc)
## expend mean -----------------------------------------------------------
mean(nondelta$supermarketwhole_expend)
sd(nondelta$supermarketwhole_expend)

mean(nondelta$supermarketfood_expend)
sd(nondelta$supermarketfood_expend)

mean(nondelta$healthfood_expend)
sd(nondelta$healthfood_expend)

mean(nondelta$convenience_expend)
sd(nondelta$convenience_expend)

mean(nondelta$online_expend)
sd(nondelta$online_expend)

mean(nondelta$discount_expend)
sd(nondelta$discount_expend)

mean(nondelta$smallstore_expend)
sd(nondelta$smallstore_expend)

mean(nondelta$farmmarket_expend)
sd(nondelta$farmmarket_expend)

mean(nondelta$directfarm_expend)
sd(nondelta$directfarm_expend)

mean(nondelta$foodbox_expend)
sd(nondelta$foodbox_expend)

mean(nondelta$mealkit_expend)
sd(nondelta$mealkit_expend)

mean(nondelta$market_expend)
sd(nondelta$market_expend)

## demographics  -----------------------------------------------------------

mean(nondelta$chainrest_expend)
sd(nondelta$chainrest_expend)

mean(nondelta$localrest_expend)
sd(nondelta$localrest_expend)

## demo vs values
affordable <- lm(afford ~ Q9 + Q5 + hispanic + native + black + rucc + income_increase + income_decrease, data = nondelta)
summary(affordable)

affordable <- lm(afford ~ Q9 + Q48 + Q5 + hispanic + native + black + rucc, data = nondelta)
summary(affordable)

health <- lm(healthy ~ Q9 + Q48 + Q5 + hispanic + native + black + rucc, data = nondelta)
summary(health)

local_grown <- lm(locally_grown ~ Q9 + Q48 + Q5 + hispanic + rucc + income_increase + income_decrease, data = nondelta)
summary(local_grown)

local_econ <- lm(local_econ ~ Q9 + Q48 + Q5 + hispanic + native + black + rucc, data = nondelta)
summary(local_econ)

access <- lm(access ~ Q9 + Q48 + Q5 + hispanic + native + black + rucc, data = nondelta)
summary(access)

organic <- lm(organic ~ Q9 + Q5 + hispanic + native + black + rucc + income_increase + income_decrease, data = delta)
summary(organic)

# delta version

affordable <- lm(afford ~ Q9 + Q48 + Q5 + hispanic + native + black + rucc, data = delta)
summary(affordable)

health <- lm(healthy ~ Q9 + Q48 + Q5 + hispanic + native + black + rucc, data = delta)
summary(health)

local_grown <- lm(locally_grown ~ Q9 + Q48 + Q5 + hispanic + native + black + rucc, data = delta)
summary(local_grown)

organic <- lm(organic ~ Q9 + Q48 + Q5 + hispanic + native + black + rucc, data = delta)
summary(organic)

local_econ <- lm(local_econ ~ Q9 + Q48 + Q5 + hispanic + native + black + rucc, data = delta)
summary(local_econ)

access <- lm(access ~ Q9 + Q48 + Q5 + hispanic + native + black + rucc, data = delta)
summary(access)







## value breakdown

delta_values <- subset(delta, select = c(1,151:166))
delta_values %<>% mutate(t2 = fv_top_three_factors) %>% separate_rows(fv_top_three_factors, sep = ",")

values_sum <- delta_values %>% group_by(fv_top_three_factors) %>%
  summarise(count = n())
names(values_sum)[names(values_sum) == "count"] <- "delta_count"

nondelta_values <- subset(nondelta, select = c(4,151:166))
nondelta_values %<>% mutate(t2 = fv_top_three_factors) %>% separate_rows(fv_top_three_factors, sep = ",")

nd_values_sum <- nondelta_values %>% group_by(fv_top_three_factors) %>%
  summarise(count = n())
names(nd_values_sum)[names(nd_values_sum) == "count"] <- "nondelta_count"

values_sum <- merge(values_sum, nd_values_sum)

write.csv(values_sum, "cleaneddata/values_summary.csv")


## expenditure details ---------------------------------------------

delta_expend <- subset(delta, select = c(1,176:177,234))
delta_expend$percent <- delta_expend$sum_expend/delta_expend$income_weekly
mean(delta_expend$percent)


