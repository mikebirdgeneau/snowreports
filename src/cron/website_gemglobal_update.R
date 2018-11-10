# GEM Global Update
require(ProjectTemplate)
sink("/dev/null")
suppressMessages(reload.project())

if(checkNewFcst(fcst="GEMGlobal")$fetch)
{
fetchGEMGlobal()
}

gc()
suppressMessages(reload.project())
sink()

plot.GEMGlobal.precip(type="snow",limit.hr=24)
plot.GEMGlobal.precip(type="snow",limit.hr=48)
plot.GEMGlobal.precip(type="snow",limit.hr=72)
plot.GEMGlobal.precip(type="snow",limit.hr=96)
plot.GEMGlobal.precip(type="snow",limit.hr=120)

system("convert -delay 110 -loop 1000 graphs/*forecastGEMGlobal_snow.png tmp/5d_forecastGEMGlobal_snow.gif")
system("mv tmp/5d_forecastGEMGlobal_snow.gif graphs/5d_forecastGEMGlobal_snow.gif")

plot.GEMGlobal.pressure(type="surface",min.hr=0,limit.hr=24)
plot.GEMGlobal.pressure(type="surface",min.hr=24,limit.hr=48)
plot.GEMGlobal.pressure(type="surface",min.hr=48,limit.hr=72)
plot.GEMGlobal.pressure(type="surface",min.hr=72,limit.hr=96)
plot.GEMGlobal.pressure(type="surface",min.hr=96,limit.hr=120)

system("convert -delay 110 -loop 1000 graphs/*forecastGEMGlobal_surfpress.png tmp/5d_forecastGEMGlobal_surfpress.gif")
system("mv tmp/5d_forecastGEMGlobal_surfpress.gif graphs/5d_forecastGEMGlobal_surfpress.gif")

plot.GEMGlobal.pressure(type="thick1000_500mb",limit.hr=24)
plot.GEMGlobal.pressure(type="thick1000_500mb",limit.hr=48,min.hr=24)
plot.GEMGlobal.pressure(type="thick1000_500mb",limit.hr=72,min.hr=48)
#plot.GEMGlobal.pressure(type="thick1000_500mb",limit.hr=96,min.hr=72)
#plot.GEMGlobal.pressure(type="thick1000_500mb",limit.hr=120,min.hr=96)

plot.GEMGlobal.pressure(type="hgt850mb",limit.hr=24)
plot.GEMGlobal.pressure(type="hgt850mb",limit.hr=48,min.hr=24)
plot.GEMGlobal.pressure(type="hgt850mb",limit.hr=72,min.hr=48)
#plot.GEMGlobal.pressure(type="hgt850mb",limit.hr=96,min.hr=72)
#system("convert -delay 110 -loop 1000 graphs/*forecastGEMGlobal_850geopot.png tmp/5d_forecastGEMGlobal_850geopot.gif")
#system("mv tmp/5d_forecastGEMGlobal_850geopot.gif graphs/5d_forecastGEMGlobal_850geopot.gif")

plot.GEMGlobal.pressure(type="hgt500mb",limit.hr=24)
plot.GEMGlobal.pressure(type="hgt500mb",limit.hr=48,min.hr=24)
plot.GEMGlobal.pressure(type="hgt500mb",limit.hr=72,min.hr=48)

plot.GEMGlobal.cloud(limit.hr=12,min.hr=0)
plot.GEMGlobal.cloud(limit.hr=24,min.hr=12)
plot.GEMGlobal.cloud(limit.hr=36,min.hr=24)
plot.GEMGlobal.cloud(limit.hr=48,min.hr=36)
plot.GEMGlobal.cloud(limit.hr=60,min.hr=48)
plot.GEMGlobal.cloud(limit.hr=72,min.hr=60)
plot.GEMGlobal.cloud(limit.hr=84,min.hr=72)
plot.GEMGlobal.cloud(limit.hr=96,min.hr=84)
plot.GEMGlobal.cloud(limit.hr=108,min.hr=96)
plot.GEMGlobal.cloud(limit.hr=120,min.hr=108)

system("convert -delay 110 -loop 1000 graphs/*forecastGEMGlobal_cloud.png tmp/5d_forecastGEMGlobal_cloud.gif")
system("mv tmp/5d_forecastGEMGlobal_cloud.gif graphs/5d_forecastGEMGlobal_cloud.gif")

plot.GEMGlobal.precip(type="rain",limit.hr=24)
plot.GEMGlobal.precip(type="rain",limit.hr=48)
plot.GEMGlobal.precip(type="rain",limit.hr=72)
plot.GEMGlobal.precip(type="rain",limit.hr=96)
plot.GEMGlobal.precip(type="rain",limit.hr=120)

system("convert -delay 110 -loop 1000 graphs/*forecastGEMGlobal_rain.png tmp/5d_forecastGEMGlobal_rain.gif")
system("mv tmp/5d_forecastGEMGlobal_rain.gif graphs/5d_forecastGEMGlobal_rain.gif")

message("[Retrieving Long Term Forecasts for resorts...]")
resort.cells<-getCells.GEMGlobal(type="resorts")
for(i in 1:nrow(poi.resorts)){ 
  writeDB.GEMGlobal.local(local=resort.cells[which(resort.cells$resort==poi.resorts$Resort[i]),])
}  

resort.cells<-getCells.GEMGlobal(type="backcountry")
for(i in 1:nrow(poi.backcountry)){ 
  writeDB.GEMGlobal.local(local=resort.cells[which(resort.cells$resort==poi.backcountry$Resort[i]),])
} 

resort.cells<-getCells.GEMGlobal(type="towns")
for(i in 1:nrow(poi.towns)){ 
  writeDB.GEMGlobal.local(local=resort.cells[which(resort.cells$resort==poi.towns$Resort[i]),])
} 

resort.cells<-getCells.GEMGlobal(type="cities")
for(i in 1:nrow(poi.cities)){ 
  writeDB.GEMGlobal.local(local=resort.cells[which(resort.cells$resort==poi.cities$Resort[i]),])
} 

pushover("Global Update Complete.")
