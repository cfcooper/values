
## summary stats


library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape)
library(magrittr)
library(formattable)
library(ggpubr)


rm(list=ls()) # caution: this clears the environment

## read in dataframes -----------------------------------------------------------

delta <- readRDS("cleaneddata/delta.rds")
nondelta <- readRDS("cleaneddata/nondelta.rds")

nondelta$income_weekly <- nondelta$income/52
delta$income_weekly <- delta$income/52

#delta$rucc <- as.numeric(delta$rucc)
#nondelta$rucc <- as.numeric(nondelta$rucc)

mergedat <- merge(delta,nondelta, all = TRUE)
## sig tests -----------------

prop.test(mergedat$locally_grown, mergedat$region_Delta, p = NULL, alternative = "two.sided",
          correct = TRUE)

## sig test for likert -----------------

t.test( formula = Q21_1 ~ region_Delta, data = mergedat, var.equal = TRUE )
t.test( formula = Q21_2 ~ region_Delta, data = mergedat, var.equal = TRUE )
t.test( formula = Q21_12 ~ region_Delta, data = mergedat, var.equal = TRUE )
t.test( formula = Q21_13 ~ region_Delta, data = mergedat, var.equal = TRUE )
t.test( formula = Q21_14 ~ region_Delta, data = mergedat, var.equal = TRUE )
t.test( formula = Q21_15 ~ region_Delta, data = mergedat, var.equal = TRUE )
t.test( formula = Q21_16 ~ region_Delta, data = mergedat, var.equal = TRUE )
t.test( formula = Q22_6 ~ region_Delta, data = mergedat, var.equal = TRUE )
t.test( formula = Q22_8 ~ region_Delta, data = mergedat, var.equal = TRUE )

mergedat$comma <- str_count(mergedat$fv_top_three_factors, ",")
comma_count <- mergedat %>% group_by(comma) %>%
  summarise(count = n())



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


## expend mean (delta) -----------------------------------------------------------
mean(delta$supermarketwhole_expend)
sd(delta$supermarketwhole_expend)

class(nondelta$supermarketwhole_expend)

mean(delta$supermarketfood_expend)
sd(delta$supermarketfood_expend)

mean(delta$healthfood_expend)
sd(delta$healthfood_expend)

mean(delta$convenience_expend)
sd(delta$convenience_expend)

mean(delta$online_expend)
sd(delta$online_expend)

mean(delta$discount_expend)
sd(delta$discount_expend)

mean(delta$smallstore_expend)
sd(delta$smallstore_expend)

mean(delta$farmmarket_expend)
sd(delta$farmmarket_expend)

mean(delta$directfarm_expend)
sd(delta$directfarm_expend)

mean(delta$foodbox_expend)
sd(delta$foodbox_expend)

mean(delta$mealkit_expend)
sd(delta$mealkit_expend)

mean(delta$market_expend)
sd(delta$market_expend)

mean(delta$chainrest_expend)
sd(delta$chainrest_expend)

mean(delta$localrest_expend)
sd(delta$localrest_expend)

## demographics  -----------------------------------------------------------

mean(nondelta$chainrest_expend)
sd(nondelta$chainrest_expend)

mean(nondelta$localrest_expend)
sd(nondelta$localrest_expend)

nondelta$Q50 <- as.numeric(nondelta$Q50)
nondelta <- nondelta[complete.cases(nondelta[ , 167]),]
mean(nondelta$Q50)

delta <- nondelta[complete.cases(delta[ , 167]),]
mean(delta$Q50)


## demo vs values
affordable <- lm(afford ~ Q9 + Q5 + hispanic + native + black + rural + income_increase + income_decrease, data = nondelta)
summary(affordable)

affordable <- lm(afford ~ Q9 + Q48 + Q5 + hispanic + native + black + rucc, data = nondelta)
summary(affordable)

health <- lm(healthy ~ Q9 + Q48 + Q5 + hispanic + native + black + rucc, data = nondelta)
summary(health)

local_grown <- lm(locally_grown ~ Q9 + Q48 + Q5 + hispanic + rural + income_increase + income_decrease, data = nondelta)
summary(local_grown)

local_econ <- lm(local_econ ~ Q9 + Q48 + Q5 + hispanic + native + black + rucc, data = nondelta)
summary(local_econ)

access <- lm(access ~ Q9 + Q48 + Q5 + hispanic + native + black + rucc, data = nondelta)
summary(access)

organic <- lm(organic ~ Q9 + Q5 + hispanic + native + black + rural + income_increase + income_decrease, data = delta)
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

columns_to_consider <- c(54:68)

delta$num_channel <- apply(delta[c(54:68)], 1, function(row) sum(row > 0))


## likert scale details ---------------------------------------------

nondelta_likert <- subset(nondelta, select = c(4,151:159))
mean(nondelta_likert$Q21_1)
mean(nondelta_likert$Q21_2)
mean(nondelta_likert$Q21_12)
mean(nondelta_likert$Q21_13)
mean(nondelta_likert$Q21_14)
mean(nondelta_likert$Q21_16)
mean(nondelta_likert$Q21_15)
mean(nondelta_likert$Q22_6)
mean(nondelta_likert$Q22_8)

sd(nondelta_likert$Q21_1)
sd(nondelta_likert$Q21_2)
sd(nondelta_likert$Q21_12)
sd(nondelta_likert$Q21_13)
sd(nondelta_likert$Q21_14)
sd(nondelta_likert$Q21_16)
sd(nondelta_likert$Q21_15)
sd(nondelta_likert$Q22_6)
sd(nondelta_likert$Q22_8)

