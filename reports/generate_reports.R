# Generate Reports for Selected Locations

message("[Generating Reports...]\n")
sink(file="/dev/null")
source("reports/revelstoke/revelstoke.R")
source("reports/lake_louise/lake_louise.R")
source("reports/canmore/canmore.R")
source("reports/regional_rockies/regional_rockies.R")
#source("reports/fernie/fernie.R")
#source("reports/panorama/panorama.R")
#source("reports/banff/banff.R")
#source("reports/emerald_lake/emerald_lake.R")
sink()