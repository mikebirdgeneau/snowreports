# Required Packages
#install.packages(c("ncdf","mapproj","fields","sp","rgdal","maptools","raster","gstat"))
require(ncdf)
require(mapproj)
require(sp)
require(rgdal)
require(gstat)
require(data.table)
require(RColorBrewer)

flipVertical <- function(x) {
  if (!inherits(x, "SpatialGridDataFrame")) stop("x must be a SpatialGridDataFrame")
  grd <- getGridTopology(x)
  idx = 1:prod(grd@cells.dim[1:2])
  m = matrix(idx, grd@cells.dim[2], grd@cells.dim[1], byrow = TRUE)[grd@cells.dim[2]:1, ]
  idx = as.vector(t(m)) 
  x@data <- x@data[idx, TRUE, drop = FALSE]
  x
}

# Get current date in UTC
date=format(as.POSIXct(Sys.time()-7*3600, tz=format(Sys.time(),"%Z")),"%Y%m%d",tz="UTC")

model0 <- nc_open("cache/ec_hrdps/ec_hrdps_000.nc")
model <- nc_open("cache/ec_hrdps/ec_hrdps.nc",readunlim=FALSE)
#dims=dim(ncvar_get(model0,"LAND_surface"))
date.fancy=format(as.POSIXct.numeric(min(ncvar_get(model0,"time")),tz="UTC",origin="1970-01-01"),"%Y-%m-%d %H:%M %Z",tz="America/Edmonton")

x <- as.vector(ncvar_get(model, "longitude"))-360
y <- as.vector(ncvar_get(model, "latitude"))
temp <- ncvar_get(model, "TMP_2maboveground")[,,]
temp=aperm(temp,c(1,2,3))
temp.max=as.vector(apply(temp,MARGIN=c(1,2),FUN=max)-273.15)
temp.min=as.vector(apply(temp,MARGIN=c(1,2),FUN=min)-273.15)
rm(temp)

snow <- ncvar_get(model,"SPRATE_surface")[,,]
snow <- aperm(snow,c(1,2,3))
snow.acc<-as.vector(apply(snow,MARGIN=c(1,2),FUN=max))
#snow <- aperm(snow,c(1,2,3))*3600
#snow.acc<-as.vector(apply(snow,MARGIN=c(1,2),FUN=sum))/(3600*24)
rm(snow)

plot.data=data.table(temp.max=temp.max,temp.min=temp.min,snow.acc=snow.acc)#,cloud.cover=cloud.cover)
rm(model,model0,temp.max,temp.min,snow.acc)#,cloud.cover)
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

# Load additional shapefiles
canada_borders<-readOGR(dsn="shapefiles/canadaborders.shp",layer="canadaborders")
canada_borders <- spTransform(canada_borders, CRS("+proj=stere +lat_0=90 +lat_ts=60 +lon_0=247 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs"))
canada_borders <- as(canada_borders, "SpatialLinesDataFrame")
roads<-readOGR(dsn="shapefiles/roads.shp",layer="roads")
roads <- spTransform(roads, CRS("+proj=stere +lat_0=90 +lat_ts=60 +lon_0=247 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs"))
roads <- as(roads, "SpatialLinesDataFrame")

# Add annotations for major cities / towns
cities=data.frame(c("Calgary",-114.078225,51.05456),
                    c("Vancouver",-123.113927,49.261226),
                    c("Edmonton",-113.5006,53.5472)
                    )
rownames(cities)<-c("City","x","y")
colnames(cities)<-NULL
cities<-data.frame(t(cities))
cities$x=as.numeric(as.character(cities$x))
cities$y=as.numeric(as.character(cities$y))
coordinates(cities)<-c('x','y')
proj4string(cities)='+proj=longlat +ellps=GRS80 +towgs84'
cities<-spTransform(cities,CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=247 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs'))

# Add annotations for resorts
resorts<-data.frame(c("Apex",-119.901258,49.383206),
                    c("Big White",-118.936465,49.718226),
                    c("Castle",-114.414854,49.318039),
                    c("Fernie",-115.088050,49.462611),
                    c("Fortress",-115.198479,50.824895),
                    c("Kicking Horse",-117.050764,51.299683),
                    c("Kimberley",-116.016485,49.682298),
                    c("Lake Louise",-116.162701,51.444026),
                    c("Marmot",-118.084851,52.800858),
                    c("Mt. Washington",-125.310683,49.741935),
                    c("Nakiska",-115.151383,50.942581),
                    c("Norquay",-115.600661,51.203489),
                    c("Panorama",-116.237474,50.457003),
                    c("Powder King",-122.613350,55.358821),
                    c("Red Mountain",-117.822502,49.104072),
                    c("Revelstoke",-118.153097,50.951819),
                    c("Shames",-128.961203,54.488983),
                    c("Silver Star",-119.061755,50.358843),
                    c("Sun Peaks",-119.885167,50.882157),
                    c("Sunshine",-115.781810,51.079066),
                    c("Whistler",-122.953433,50.107467),
                    c("Whitewater",-117.145346,49.443357),
                    c("Whitefish",-114.353560,48.486294)#,
                    #c("Roger's Pass",-117.52000,51.300418)
                    )
rownames(resorts)<-c("Resort","x","y")
colnames(resorts)<-NULL
resorts<-data.frame(t(resorts))
resorts$x=as.numeric(as.character(resorts$x))
resorts$y=as.numeric(as.character(resorts$y))
#write.csv(resorts,file="data/resorts.csv",row.names=FALSE)
coordinates(resorts)<-c('x','y')
proj4string(resorts)='+proj=longlat +ellps=GRS80 +towgs84'
resorts<-spTransform(resorts,CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=247 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs'))

# Setup Plot
rgb.palette=colorRampPalette(c("lightslateblue","snow1","snow2","snow3","seagreen","orange","firebrick"), space = "rgb")
rgb.palette=colorRampPalette(rev(brewer.pal(n=9,name="Spectral")))
plotlim=data.frame(x=c(-128,-112),y=c(47,56.5))
coordinates(plotlim)<-c('x','y')
proj4string(plotlim)='+proj=longlat +ellps=GRS80 +towgs84'
plotlim<-spTransform(plotlim,CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=247 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs'))
rm(x,y,coords,flipVertical)

p1.sm=spplot(test2,c("snow.acc"),names.attr=c("Snow Accumulation"),
	  col.regions=colorRampPalette(c("white", "#C5E9FF", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598","#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F", "#9E0142")),
          at=c(0,1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80),
          colorkey=list(labels=list(at=c(1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80),labels=c(1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80))),
          main=paste0("24h Snowfall Forecast (cm) at ",date.fancy),
          xlab="Snow-Reports.ca - Source: Environment Canada, GeoGratis",
          scales=list(draw=FALSE),
          xlim=c(plotlim@bbox[1],plotlim@bbox[3]),
          ylim=c(plotlim@bbox[2],plotlim@bbox[4]),
          sp.layout=list(c('sp.points',cities,col="black",pch=21,cex=0.8,lwd=1,fill="grey"),
                         c('sp.lines',canada_borders,col="darkgrey",lwd=2),
                         c('sp.points',resorts,col="black",pch=15,cex=0.5),
                         c('sp.lines',roads,col="grey",lwd=1,alpha=0.5),
                         do.call("list",list("sp.text",coordinates(resorts),resorts$Resort,pos=4,cex=0.65))
          )
)
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
          sp.layout=list(c('sp.points',cities,col="black",pch=21,cex=0.8,lwd=1,fill="grey"),
                         c('sp.lines',canada_borders,col="darkgrey",lwd=2),
                         c('sp.points',resorts,col="black",pch=15,cex=0.5),
                         c('sp.lines',roads,col="grey",lwd=1,alpha=0.5),
                         do.call("list",list("sp.text",coordinates(resorts),resorts$Resort,pos=4,cex=0.7))
                         )
          )

png(filename="graphs/24h_forecast_tmp.png",width=700,height=790,units="px",res=72,type="cairo")
print(p1,position=c(0,0.33,1,1),split=c(1,1,1,1),more=TRUE)
rm(p1)
gc()
p2=spplot(test2,c("temp.min"),names.attr=c("Forecast Low (°C)"),
          col.regions=rgb.palette,
          at=seq(-30,30,by=1),
          main=paste0("24h Forecast Low (°C)"),
          scales=list(draw=FALSE),
          xlim=c(plotlim@bbox[1],plotlim@bbox[3]),
          ylim=c(plotlim@bbox[2],plotlim@bbox[4]),
          sp.layout=list(c('sp.points',cities,col="black",pch=21,cex=0.6,lwd=1,fill="grey"),
                         c('sp.lines',canada_borders,col="lightgrey",lwd=1),
                         c('sp.points',resorts,col="black",pch=15,cex=0.4),
                         c('sp.lines',roads,col="grey",lwd=1,alpha=0.5)))

print(p2,position=c(0,0.02,0.5,0.33),split=c(1,1,1,1),more=TRUE)
rm(p2)
gc()
p3=spplot(test2,c("temp.max"),names.attr=c("Forecast High (C)"),
         col.regions=rgb.palette,
         at=seq(-30,30,by=1),
         main=paste0("24h Forecast High (C)"),
         scales=list(draw=FALSE),
         xlim=c(plotlim@bbox[1],plotlim@bbox[3]),
         ylim=c(plotlim@bbox[2],plotlim@bbox[4]),
         sp.layout=list(c('sp.points',cities,col="black",pch=21,cex=0.6,lwd=1,fill="grey"),
                        c('sp.lines',canada_borders,col="lightgrey",lwd=1),
                        c('sp.points',resorts,col="black",pch=15,cex=0.4),
                        c('sp.lines',roads,col="grey",lwd=1,alpha=0.5)))

print(p3,position=c(0.5,0.02,1,0.33),split=c(1,1,1,1),more=FALSE)
rm(p3)
gc()
dev.off()

# Update live version on server
system("mv graphs/24h_forecast_sm_tmp.png graphs/24h_forecast_sm.png")
system("mv graphs/24h_forecast_tmp.png graphs/24h_forecast.png")
