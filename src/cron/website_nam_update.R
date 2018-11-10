# NAM (NOAA / NCEP) Forecast Wrapper (v2)
require(ProjectTemplate)
rm(list=ls())
gc()
suppressMessages(reload.project())

getNAMForecast()

plot.NAM.precip(type="snow",output=TRUE,max.hr=24)
plot.NAM.precip(type="snow",output=TRUE,max.hr=48)
plot.NAM.precip(type="snow",output=TRUE,max.hr=72)
plot.NAM.precip(type="snow",output=TRUE)

plot.NAM.precip(type="rain",output=TRUE,max.hr=24)
plot.NAM.precip(type="rain",output=TRUE,max.hr=48)
plot.NAM.precip(type="rain",output=TRUE,max.hr=72)
plot.NAM.precip(type="rain",output=TRUE)


rm(list=ls())
gc()
suppressMessages(reload.project())

message("[Plotting Resort Forecasts & Soundings.]")
resort.cells<-getCells.NAM(type="resorts")
for(i in 1:nrow(resort.cells)){ 
  cat(paste0("[",resort.cells$resort[i]," local forecast...]\n"))
  plot.NAM.local(poi=resort.cells$resort[i],type="resorts",limit.data=FALSE) 
  writeDB.NAM.local(local=resort.cells[which(resort.cells$resort==resort.cells$resort[i]),])
}  

message("[Plotting Backcountry Forecasts & Soundings.]")
resort.cells<-getCells.NAM(type="backcountry")
for(i in 1:nrow(resort.cells)){ 
  cat(paste0("[",resort.cells$resort[i]," local forecast...]\n"))
  plot.NAM.local(poi=resort.cells$resort[i],type="backcountry",limit.data=FALSE) 
  writeDB.NAM.local(local=resort.cells[which(resort.cells$resort==resort.cells$resort[i]),])
} 

message("[Plotting City Forecasts & Soundings.]")
resort.cells<-getCells.NAM(type="cities")
for(i in 1:nrow(resort.cells)){ 
  cat(paste0("[",resort.cells$resort[i]," local forecast...]\n"))
  plot.NAM.local(poi=resort.cells$resort[i],type="cities",limit.data=FALSE) 
  writeDB.NAM.local(local=resort.cells[which(resort.cells$resort==resort.cells$resort[i]),])
} 

message("[Plotting Town Forecasts & Soundings.]")
resort.cells<-getCells.NAM(type="towns")
for(i in 1:nrow(poi.towns)){
  cat(paste0("[",resort.cells$resort[i]," local forecast...]\n"))
  plot.NAM.local(poi=resort.cells$resort[i],type="towns",limit.data=FALSE) 
  writeDB.NAM.local(local=resort.cells[which(resort.cells$resort==resort.cells$resort[i]),])
} 

source("lib/pushoverr.R")
pushover("NAM Update Complete.")
