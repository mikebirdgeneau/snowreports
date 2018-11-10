# GEM Regional 48h Forecast Wrapper (v2)
require(ProjectTemplate)
rm(list=ls())
gc()
suppressMessages(reload.project())

fetchGEMRegional()

plot.GEMRegional.precip(type="snow",output=TRUE)
plot.GEMRegional.precip(type="snow",output=TRUE,limit.hr=24)
plot.GEMRegional.clouds(limit.hr=24)

rm(list=ls())
gc()
suppressMessages(reload.project())

message("[Plotting Resort Forecasts & Soundings.]")
resort.cells<-getCells.GEMRegional(type="resorts")
for(i in 1:nrow(resort.cells)){ 
  cat(paste0("[",resort.cells$resort[i]," local forecast...]\n"))
  plot.GEMRegional.local(poi=resort.cells$resort[i],type="resorts",limit.data=FALSE) 
  cat(paste0("[",resort.cells$resort[i]," atmospheric sounding...]\n"))
  soundingData <- get.GEMRegional.soundingData(poi=resort.cells$resort[i],type="resorts")
  plot.GEMRegional.sounding(poi=resort.cells$resort[i],type="resorts",soundingData = soundingData)
  writeDB.GEMRegional.soundingData(poi=resort.cells$resort[i],type="resorts",soundingData = soundingData)
  writeDB.GEMRegional.local(local=resort.cells[which(resort.cells$resort==resort.cells$resort[i]),])
}  

message("[Plotting Backcountry Forecasts & Soundings.]")
resort.cells<-getCells.GEMRegional(type="backcountry")
for(i in 1:nrow(resort.cells)){ 
  cat(paste0("[",resort.cells$resort[i]," local forecast...]\n"))
  plot.GEMRegional.local(poi=resort.cells$resort[i],type="backcountry",limit.data=FALSE) 
  cat(paste0("[",resort.cells$resort[i]," atmospheric sounding...]\n"))
  soundingData <- get.GEMRegional.soundingData(poi=resort.cells$resort[i],type="backcountry")
  plot.GEMRegional.sounding(poi=resort.cells$resort[i],type="backcountry",soundingData = soundingData)
  writeDB.GEMRegional.soundingData(poi=resort.cells$resort[i],type="backcountry",soundingData = soundingData)
  writeDB.GEMRegional.local(local=resort.cells[which(resort.cells$resort==resort.cells$resort[i]),])
} 

message("[Plotting City Forecasts & Soundings.]")
resort.cells<-getCells.GEMRegional(type="cities")
for(i in 1:nrow(resort.cells)){ 
  cat(paste0("[",resort.cells$resort[i]," local forecast...]\n"))
  plot.GEMRegional.local(poi=resort.cells$resort[i],type="cities",limit.data=FALSE) 
  cat(paste0("[",resort.cells$resort[i]," atmospheric sounding...]\n"))
  soundingData <- get.GEMRegional.soundingData(poi=resort.cells$resort[i],type="cities")
  plot.GEMRegional.sounding(poi=resort.cells$resort[i],type="cities",soundingData = soundingData)
  writeDB.GEMRegional.soundingData(poi=resort.cells$resort[i],type="cities",soundingData = soundingData)
  writeDB.GEMRegional.local(local=resort.cells[which(resort.cells$resort==resort.cells$resort[i]),])
} 

message("[Plotting Town Forecasts & Soundings.]")
resort.cells<-getCells.GEMRegional(type="towns")
for(i in 1:nrow(resort.cells)){
  cat(paste0("[",resort.cells$resort[i]," local forecast...]\n"))
  plot.GEMRegional.local(poi=resort.cells$resort[i],type="towns",limit.data=FALSE) 
  cat(paste0("[",resort.cells$resort[i]," atmospheric sounding...]\n"))
  soundingData <- get.GEMRegional.soundingData(poi=resort.cells$resort[i],type="towns")
  plot.GEMRegional.sounding(poi=resort.cells$resort[i],type="towns",soundingData = soundingData)
  writeDB.GEMRegional.soundingData(poi=resort.cells$resort[i],type="towns",soundingData = soundingData)
  writeDB.GEMRegional.local(local=resort.cells[which(resort.cells$resort==resort.cells$resort[i]),])
} 

#reload.project()
#plot.GEMRegional.precip(type="rain",output=TRUE)


for(i in 1:nrow(dt.local.cell.info)){
  loc<-dt.local.cell.info[i]
  test<-fetchData.GEMRegional(poi = loc$resort,type = loc$type)
  tmpfile<-tempfile(pattern = paste0(round(as.numeric(Sys.time()),0),"_-_",str_replace_all(test$poi," ","_")),fileext = ".csv")
  write.csv(test$local.data,file=tmpfile,row.names=FALSE)
  system(paste0("s3cmd put ",tmpfile," s3://snow-reports.ca/weather_forecast_data/localdata/"))
  unlink(tmpfile)
  rm(tmpfile)
  
  test$elev.data<-merge(test$elev.data,subset(dt.local.cell.info,select=-c(type,geocoord,fcst.elev)),by=c("resort"),all.x=TRUE)
  tmpfile<-tempfile(pattern = paste0(round(as.numeric(Sys.time()),0),"_-_",str_replace_all(test$poi," ","_")),fileext = ".csv")
  write.csv(test$elev.data,file=tmpfile,row.names=FALSE)
  system(paste0("s3cmd put ",tmpfile," s3://snow-reports.ca/weather_forecast_data/elevationdata/"))
  unlink(tmpfile)
  rm(tmpfile)
  rm(test)
}



source("reports/regional_rockies/regional_rockies.R")

source("lib/pushoverr.R")
pushover("Regional Update Complete.")
