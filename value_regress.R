
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

write.csv(delta, "delta_raw.csv")

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

super_health_reg1 <- lm(healthfood_expend ~ income_weekly + value_4, data = delta)
summary(super_health_reg1)


super_health_reg1 <- lm(healthfood_expend ~ income_weekly + value_5, data = delta)
summary(super_health_reg1)


r <- lm(online_expend ~ income_weekly + value_3, data = delta)
summary(r)

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

