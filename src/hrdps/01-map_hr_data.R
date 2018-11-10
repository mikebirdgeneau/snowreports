# Required Packages
#install.packages(c("ncdf","mapproj","fields","sp","rgdal","maptools","raster","gstat"))
require(ncdf)
require(mapproj)
require(sp)
require(rgdal)
require(gstat)
require(data.table)
require(RColorBrewer)


model0 <- nc_open("cache/ec_hrdps/ec_hrdps_000.nc")
model <- nc_open("cache/ec_hrdps/ec_hrdps.nc",readunlim=FALSE)
date.fancy=format(as.POSIXct.numeric(min(ncvar_get(model0,"time")),tz="GMT",origin="1970-01-01"),"%Y-%m-%d %H:%M %Z",tz="America/Edmonton")

x <- as.vector(ncvar_get(model, "longitude"))-360
y <- as.vector(ncvar_get(model, "latitude"))
temp <- ncvar_get(model, "TMP_2maboveground")[,,]
temp=aperm(temp,c(1,2,3))
temp.max=as.vector(apply(temp,MARGIN=c(1,2),FUN=max)-273.15)
temp.min=as.vector(apply(temp,MARGIN=c(1,2),FUN=min)-273.15)
temp.mean=as.vector(apply(temp,MARGIN=c(1,2),FUN=mean)-273.15)
rm(temp)

snow <- ncvar_get(model,"SPRATE_surface")[,,]
snow <- aperm(snow,c(1,2,3))
snow.acc<-as.vector(apply(snow,MARGIN=c(1,2),FUN=max))
rm(snow)

cloud.cover <- ncvar_get(model,"TCDC_surface")[,,]
cloud.cover <- aperm(cloud.cover,c(1,2,3))

hgt800mb<-ncvar_get(model,"HGT_1000mb")
hgt800mb<-aperm(hgt800mb,c(1,2,3))

plot.data=data.table(temp.max=temp.max,temp.min=temp.min,snow.acc=snow.acc,cloud.cover=cloud.cover,hgt800=hgt800mb)
rm(model,model0,temp.max,temp.min,snow.acc,cloud.cover,hgt800mb)
gc()
coords=data.frame(x=x,y=y)
coordinates(coords)=~x+y
proj4string(coords)<-CRS("+proj=latlong")
grid=SpatialGrid(GridTopology(bbox(coords)[,1],c(2500,2500),c(685,485)))
test=SpatialPointsDataFrame(coords=coords,data=plot.data,
        proj4string=CRS("+proj=latlong"))
test.merc <- spTransform(test,CRS("+proj=stere +lat_0=90 +lat_ts=60 +lon_0=247 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs"))
grid<-points2grid(test.merc)
test2<-SpatialGridDataFrame(grid,plot.data)
rm(grid,plot.data,test,test.merc)
gc()
test2=flipVertical(test2)
save(test2,file="output/kml_data.Rda")

# Load Shapefiles
loadShapefiles(crs=proj.HRDPS)

# Add annotations for major cities / towns & Resorts
cities<-spTransform(poi.cities,CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=247 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs'))
resorts<-spTransform(poi.resorts,CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=247 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs'))


# Setup Plot
rgb.palette=colorRampPalette(c("lightslateblue","snow1","snow2","snow3","seagreen","orange","firebrick"), space = "rgb")
rgb.palette=colorRampPalette(rev(brewer.pal(n=9,name="Spectral")))
plotlim=data.frame(x=c(-128,-112),y=c(47,56.5))
coordinates(plotlim)<-c('x','y')
proj4string(plotlim)='+proj=longlat +ellps=GRS80 +towgs84'
plotlim<-spTransform(plotlim,CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=247 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs'))
rm(x,y,coords)

splayoutList<-list(c('sp.points',cities,col="black",pch=21,cex=0.8,lwd=1,fill="grey"),
     c('sp.lines',shpCanBorders,col="darkgrey",lwd=2),
     c('sp.points',resorts,col="black",pch=15,cex=0.5),
     c('sp.lines',shpCanRoads,col="grey",lwd=1,alpha=0.5),
     c('sp.lines',shpUSBorders,col="grey",lwd=1.25),
     do.call("list",list("sp.text",coordinates(resorts),resorts$Resort,pos=4,cex=0.65))
)

p1.sm=spplot(test2,c("snow.acc"),names.attr=c("Snow Accumulation"),
	  col.regions=colorRampPalette(c("white", "#C5E9FF", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598","#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F", "#9E0142")),
          at=c(0,1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80),
          colorkey=list(labels=list(at=c(1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80),labels=c(1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80))),
          main=paste0("24h Snowfall Forecast (cm) at ",date.fancy),
          xlab="Snow-Reports.ca - Source: Environment Canada, GeoGratis",
          scales=list(draw=FALSE),
          xlim=c(plotlim@bbox[1],plotlim@bbox[3]),
          ylim=c(plotlim@bbox[2],plotlim@bbox[4]),
          sp.layout=splayoutList)

png(filename="graphs/24h_forecast_sm_tmp.png",width=720,height=480,units="px",res=96,type="cairo")
print(p1.sm)
dev.off()
rm(p1.sm)

p1=spplot(test2,c("snow.acc"),names.attr=c("Snow Accumulation"),
          col.regions=colorRampPalette(c("white", "#C5E9FF", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598","#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F", "#9E0142")),
          at=c(0,1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80),
          colorkey=list(labels=list(at=c(1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80),labels=c(1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80))),
          main=paste0("24h High-Res. Snowfall Forecast (cm) for ",date.fancy),
          xlab="Snow-Reports.ca - Forecast Data: Environment Canada (Regional HRDPS West), (Snow Density: 100kg/m3) \n Map: GeoGratis © Department of Natural Resources Canada. All rights reserved.",
          scales=list(draw=FALSE),
          xlim=c(plotlim@bbox[1],plotlim@bbox[3]),
          ylim=c(plotlim@bbox[2],plotlim@bbox[4]),
          sp.layout=splayoutList
          )

png(filename="graphs/24h_forecast_lg_tmp.png",width=2880,height=1800,units="px",res=150,type="cairo")
print(p1)
dev.off()

png(filename="graphs/24h_forecast_tmp.png",width=1440,height=1024,units="px",res=150,type="cairo")
print(p1)
dev.off()

p2=spplot(test2,c("temp.min"),names.attr=c("Forecast Low (°C)"),
          col.regions=rgb.palette,
          pretty=TRUE,
          contour=FALSE,
          at=c(seq(floor(min(test2@data$temp.min)/5)*5,ceiling(max(test2@data$temp.max)/5)*5,by=2.5)),
          main=paste0("24h Forecast Low (°C)"),
          scales=list(draw=FALSE),
          xlim=c(plotlim@bbox[1],plotlim@bbox[3]),
          ylim=c(plotlim@bbox[2],plotlim@bbox[4]),
          sp.layout=splayoutList
          )

png(filename="graphs/24h_forecast_low.png",width=700,height=465,units="px",res=72,type="cairo")
print(p2)
dev.off()
rm(p2)
gc()

p3=spplot(test2,c("temp.max"),names.attr=c("Forecast High (C)"),
         col.regions=rgb.palette,
          at=c(seq(floor(min(test2@data$temp.min)/5)*5,ceiling(max(test2@data$temp.max)/5)*5,by=2.5)),
          main=paste0("24h Forecast High (C)"),
         scales=list(draw=FALSE),
         xlim=c(plotlim@bbox[1],plotlim@bbox[3]),
         ylim=c(plotlim@bbox[2],plotlim@bbox[4]),
          sp.layout=splayoutList
          )

png(filename="graphs/24h_forecast_high.png",width=700,height=465,units="px",res=72,type="cairo")
print(p3)
dev.off()
rm(p3)
gc()

# Update live version on server
system("mv graphs/24h_forecast_sm_tmp.png graphs/24h_forecast_sm.png")
system("mv graphs/24h_forecast_lg_tmp.png graphs/24h_forecast_lg.png")

system("mv graphs/24h_forecast_tmp.png graphs/24h_forecast.png")
