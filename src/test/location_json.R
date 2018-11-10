# Write JSON for locations, based on forecast data in DB:

# Fetch Data from DB:
con<-dbConnect(MySQL(),user="snowreports",password="dbpass",dbname="snowfall",host="localhost")

max.date <- dbGetQuery(con,"SELECT MAX(`dmax`) FROM `ecforecast48` WHERE `fcst.day`=1")
max.date2 <- dbGetQuery(con,"SELECT MAX(`dmax`) FROM `ecforecast48` WHERE `fcst.day`=2")

ecregional48 <- data.table(dbGetQuery(con,sprintf("SELECT DISTINCT * FROM `ecforecast48` WHERE dmax = '%s' OR dmax = '%s'",max.date,max.date2)))

max.date <- dbGetQuery(con,"SELECT MAX(`fcst.date`) FROM `ec_gem_regional_sounding`")
ecregionalsounding <- data.table(dbGetQuery(con,sprintf("SELECT DISTINCT * FROM `ec_gem_regional_sounding` WHERE `fcst.date` = '%s'",max.date)))

dbDisconnect(con)

# 