
#zip to zcta pop density conversions


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

zcta_pop <- read.csv("zcta/zcta_population.csv")
zcta_geo <- read.csv("zcta/zcta_geo.csv")
zcta_zip <- read.csv("zcta/zip_zcta.csv")

zcta_dense <- merge(zcta_geo, zcta_pop, by = c("GEOID", "ZCTA"))
zcta_dense <- subset(zcta_dense, select = -c(5:6))
zcta_dense <- unique(zcta_dense)

zcta_dense$ZCTA <- gsub("ZCTA5 ", "", zcta_dense$ZCTA)

zcta_merge <- merge(zcta_dense, zcta_zip, by = "ZCTA", all = FALSE)
zcta_merge$area_land <- zcta_merge$area_land*3.861E-7
zcta_merge$dense <- zcta_merge$population/zcta_merge$area_land

zcta_merge <- subset(zcta_merge, select = c(1,7,9))

write.csv(zcta_merge,"zcta/zip_density.csv")



