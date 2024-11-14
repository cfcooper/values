
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
nondelta <- readRDS("cleaneddata/nondelta.rds")
write.csv(delta, "delta_raw.csv")

## nondelta regs  -----------------------------------------------------------

income_reg <- lm(sum_expend ~income, data = nondelta)
summary(income_reg)

hh_reg <- lm(sum_expend ~Q49, data = nondelta)
summary(hh_reg)

hh_i_reg <- lm(sum_expend ~Q49 + income, data = nondelta)
summary(hh_i_reg)

foodsecure_reg <- lm(sum_expend ~foodsecure, data = nondelta)
summary(foodsecure_reg)

income_reg2 <- lm(sum_expend ~income + value_1 + value_2 + value_3 + value_4 +
                    value_5 + value_6 + value_7, data = nondelta)
summary(income_reg2)

hh_reg2 <- lm(sum_expend ~Q49 + value_1 + value_2 + value_3 + value_4 +
                value_5 + value_6 + value_7, data = nondelta)
summary(hh_reg2)

nondelta$income_weekly <- nondelta$income/52


# start over reg -----------------------------------------------------------

income_reg <- lm(sum_expend ~income, data = delta)
summary(income_reg)

hh_reg <- lm(sum_expend ~Q49, data = delta)
summary(hh_reg)

hh_i_reg <- lm(sum_expend ~Q49 + income, data = delta)
summary(hh_i_reg)

foodsecure_reg <- lm(sum_expend ~foodsecure, data = delta)
summary(foodsecure_reg)

income_reg2 <- lm(sum_expend ~income + value_1 + value_2 + value_3 + value_4 +
                    value_5 + value_6 + value_7, data = delta)
summary(income_reg2)

hh_reg2 <- lm(sum_expend ~Q49 + value_1 + value_2 + value_3 + value_4 +
               value_5 + value_6 + value_7, data = delta)
summary(hh_reg2)

hh_reg3 <- lm(sum_expend ~Q49 + income + value_1 + value_2 + value_3 + value_4 +
                value_5 + value_6 + value_7, data = delta)
summary(hh_reg3)

## non delta channels ---------------------------------------------------------

super_whole_reg <- lm(supermarketwhole_expend ~ income_weekly + value_1 + value_2 + value_3 + value_4 +
                        value_5 + value_6 + value_7, data = nondelta)
summary(super_whole_reg)

super_whole_reg <- lm(supermarketwhole_expend ~ income_weekly + value_2, data = nondelta)
summary(super_whole_reg)

super_whole_reg <- lm(supermarketwhole_expend ~ income_weekly + value_4, data = nondelta)
summary(super_whole_reg)

health_reg4 <- lm(healthfood_expend ~ income_weekly + value_4, data = nondelta)
summary(health_reg4)

health_reg2 <- lm(healthfood_expend ~ income_weekly + value_2, data = nondelta)
summary(health_reg2)

health_reg7 <- lm(healthfood_expend ~ income_weekly + value_7, data = nondelta)
summary(health_reg7)

conv_reg <- lm(convenience_expend ~ income_weekly + value_7, data = nondelta)
summary(conv_reg)

conv_reg2 <- lm(convenience_expend ~ income_weekly + value_2, data = nondelta)
summary(conv_reg2)

conv_reg3 <- lm(convenience_expend ~ income_weekly + value_5, data = nondelta)
summary(conv_reg3)


farmmark_reg <- lm(farmmarket_expend ~ income_weekly + value_4, data = nondelta)
summary(farmmark_reg)


# P of income -----------------------------------------------------------

super_whole_reg <- lm(supermarketwhole_expend_p ~foodsecure + value_1 + value_2 + value_3 + value_4 +
                        value_5 + value_6 + value_7 + sumPCE + Q49 + rural + urban, data = delta)
summary(super_whole_reg)


super_health_reg <- lm(healthfood_expend_p ~foodsecure + value_1 + value_2 + value_3 + value_4 +
                         value_5 + value_6 + value_7 + sumPCE + Q49 + rural + urban, data = delta)
summary(super_health_reg)


conv_reg <- lm(convenience_expend_p ~foodsecure + value_1 + value_2 + value_3 + value_4 +
                          value_5 + value_6 + value_7 + sumPCE + Q49 + rural + urban, data = delta)
summary(conv_reg)


farmmark_reg <- lm(farmmarket_expend_p ~foodsecure + value_1 + value_2 + value_3 + value_4 +
                     value_5 + value_6 + value_7 + sumPCE + Q49 + rural + urban, data = delta)
summary(farmmark_reg)



# $ amount -----------------------------------------------------------

delta$income_weekly <- delta$income/52

super_whole_reg <- lm(supermarketwhole_expend ~ income_weekly + value_1 + value_2 + value_3 + value_4 +
                        value_5 + value_6 + value_7, data = delta)
summary(super_whole_reg)



super_health_reg <- lm(healthfood_expend ~ income_weekly + value_1 + value_2 + value_3 + value_4 +
                         value_5 + value_6 + value_7, data = delta)
summary(super_health_reg)


conv_reg <- lm(convenience_expend ~ income_weekly + value_1 + value_2 + value_3 + value_4 +
                 value_5 + value_6 + value_7, data = delta)
summary(conv_reg)


farmmark_reg <- lm(farmmarket_expend ~ income_weekly + value_1 + value_2 + value_3 + value_4 +
                     value_5 + value_6 + value_7, data = delta)
summary(farmmark_reg)


directfarm_reg <- lm(directfarm_expend ~ income_weekly + value_1 + value_2 + value_3 + value_4 +
                       value_5 + value_6 + value_7, data = delta)
summary(directfarm_reg)

discount_reg <- lm(discount_expend ~ income_weekly + value_1 + value_2 + value_3 + value_4 +
                     value_5 + value_6 + value_7, data = delta)
summary(discount_reg)


supermarketfood_reg <- lm(supermarketfood_expend ~ income_weekly + value_1 + value_2 + value_3 + value_4 +
                     value_5 + value_6 + value_7, data = delta)
summary(supermarketfood_reg)


print(colMeans(delta))



# % of expend amount -----------------------------------------------------------

super_whole_reg <- lm(supermarketwhole_expend_t ~foodsecure + value_1 + value_2 + value_3 + value_4 +
                        value_5 + value_6 + value_7 + income + sumPCE + Q49 + rural + urban, data = delta)
summary(super_whole_reg)



super_health_reg <- lm(healthfood_expend_t ~foodsecure + value_1 + value_2 + value_3 + value_4 +
                         value_5 + value_6 + value_7 + income + sumPCE + Q49 + rural + urban, data = delta)
summary(super_health_reg)


conv_reg <- lm(convenience_expend_t ~foodsecure + value_1 + value_2 + value_3 + value_4 +
                 value_5 + value_6 + value_7 + income + sumPCE + Q49 + rural + urban, data = delta)
summary(conv_reg)


farmmark_reg <- lm(farmmarket_expend_t ~foodsecure + value_1 + value_2 + value_3 + value_4 +
                     value_5 + value_6 + value_7 + income + sumPCE + Q49 + rural + urban, data = delta)
summary(farmmark_reg)


directfarm_reg <- lm(directfarm_expend_t ~foodsecure + value_1 + value_2 + value_3 + value_4 +
                     value_5 + value_6 + value_7 + income + sumPCE, data = delta)
summary(directfarm_reg)


discount_reg <- lm(discount_expend_t ~foodsecure + value_1 + value_2 + value_3 + value_4 +
                       value_5 + value_6 + value_7 + income + sumPCE, data = delta)
summary(discount_reg)

