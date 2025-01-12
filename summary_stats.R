
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















