#setwd("~/snow-reports.ca/src/basins/")
library("stringr")

source("lib/pushoverr.R")
source("src/basins/ab_basin_data.R")
source("src/basins/bc_basin_data.R")

pushover("Snow Pillow Update Complete.")
