# HRDPS Wrapper (v2)
library(ProjectTemplate)
library(RPushbullet)

suppressMessages(reload.project())
fetchHRDPS()

plot.HRDPS.precipitation(type="snow",output=TRUE)
plot.HRDPS.cloud(output=TRUE)
plot.HRDPS.temperature(fun="min",filename="24h_forecast_low")
plot.HRDPS.temperature(fun="max",filename="24h_forecast_high")

resort.cells<-getCells.HRDPS(type="resorts")
for(i in 1:nrow(resort.cells)){ 
  local<-resort.cells[i,]
  plot.HRDPS.local(local=local) 
  writeDB.HRDPS.local(local=local)
}  

resort.cells<-getCells.HRDPS(type="backcountry")
for(i in 1:nrow(resort.cells)){ 
  local<-resort.cells[i,]
  plot.HRDPS.local(local=local) 
  writeDB.HRDPS.local(local=local)
}  

resort.cells<-getCells.HRDPS(type="cities")
for(i in 1:nrow(resort.cells)){ 
  local<-resort.cells[i,]
  plot.HRDPS.local(local=local) 
  writeDB.HRDPS.local(local=local)
} 
resort.cells<-getCells.HRDPS(type="towns")
for(i in 1:nrow(resort.cells)){ 
  local<-resort.cells[i,]
  plot.HRDPS.local(local=local)
  writeDB.HRDPS.local(local=local)
} 

plot.HRDPS.precipitation(type="rain",output=TRUE)

suppressMessages(reload.project())
gc()

message("[Updating Snowfall Animation...]")
animate.HRDPS.snow()

suppressMessages(reload.project())
gc()

message("[Updating Cloud Animation...]")
animate.HRDPS.cloud()
message("[Done.]")

# TODO: Save full forecast dataset into DB for building advanced models. (local saved above)

# Check Files
#source("src/qaqc/file_update_check.R")

pushover("High Res Update Complete.")
pbPost(type = "note","Snow-Reports.ca: High Res. Update Complete.",body = "High Res. Update Complete.")


#source("src/standalone/pushover_resorts.R")
source("src/standalone/pushbullet_resorts.R")

source("src/twitter_alerts.R")
