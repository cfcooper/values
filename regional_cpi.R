

##regional CPI work


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

regions <- read.csv("regions.csv")
state_region <- read.csv("state_region.csv")
cpi <- read.csv("regionalcpi.csv")

regions <- merge(state_region, cpi)
write.csv(regions, "cleaneddata/regional_cpi.csv")
