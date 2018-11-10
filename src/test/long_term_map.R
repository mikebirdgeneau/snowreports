# Long Term Global Forecast Processing

# Open Model
model0 <- nc_open("cache/ec_gemglobal/ec_gemglobal_000.nc")
model <- nc_open("cache/ec_gemglobal/ec_gemglobal.nc",readunlim=FALSE)
#dims=dim(ncvar_get(model0,"LAND_surface"))
date.fancy=format(as.POSIXct.numeric(min(ncvar_get(model0,"time")),tz="UTC",origin="1970-01-01"),"%Y-%m-%d %H:%M %Z",tz="America/Edmonton")

x <- as.vector(ncvar_get(model, "longitude"))-360
y <- as.vector(ncvar_get(model, "latitude"))
t <- as.vector(ncvar_get(model, "time"))
temp <- ncvar_get(model, "TMP_2maboveground")[,,]
temp=aperm(temp,c(1,2,3))
temp.max=as.vector(apply(temp,MARGIN=c(1,2),FUN=max)-273.15)
temp.min=as.vector(apply(temp,MARGIN=c(1,2),FUN=min)-273.15)
rm(temp)

snow <- ncvar_get(model,"SPRATE_surface")[,,]
snow <- aperm(snow,c(1,2,3))
snow.acc<-as.vector(apply(snow,MARGIN=c(1,2),FUN=max))
rm(snow)

cloud <- ncvar_get(model,"TCDC_surface")[,,]
cloud <- aperm(cloud,c(1,2,3))
cloud.cover<-as.vector(apply(cloud,MARGIN=c(1,2),FUN=mean))
rm(cloud)

acc <- ncvar_get(model,"APCP_surface")[,,]
acc <- aperm(acc,c(1,2,3))
acc.precip<-as.vector(apply(acc,MARGIN=c(1,2),FUN=max))
rm(acc)

#wind<-ncvar_get(model,"WIND_10maboveground")[,,]
#wind<-aperm(wind,c(1,2,3))
#wind<-as.vector(apply(wind,MARGIN=c(1,2),FUN=mean))

#wdir<-ncvar_get(model,"WDIR_10maboveground")[,,]
#wdir<-aperm(wdir,c(1,2,3))
#wdir<-as.vector(apply(wdir,MARGIN=c(1,2),FUN=mean))

plot.data=data.table(temp.max=temp.max,temp.min=temp.min,snow.acc=snow.acc,cloud.cover=cloud.cover,acc.precip=acc.precip)
rm(model,model0,temp.max,temp.min,snow.acc,cloud.cover)
gc()
coords=expand.grid(x=x,y=y)
coordinates(coords)=~x+y
proj4string(coords)<-CRS("+proj=latlong")
coords<-spTransform(coords,CRS("+proj=latlong"))
grid=SpatialGrid(GridTopology(bbox(coords)[,1],c(1,1),c(1500,751)))
grid2<-expand.grid(x=seq(-128,-112,by=0.1),y=seq(47,56.5,by=0.1))
coordinates(grid2)=~x+y
gridded(grid2)<-TRUE
test2<-SpatialGridDataFrame(grid,plot.data)

test2<-flipVertical(test2)
#vgm.naefs<-variogram(snow.acc~x+y,test2)
#vgm.fit<-fit.variogram(vgm.naefs,vgm(11.9, "Exp", 50))
#plot(vgm.naefs,vgm.fit)
#test3<-krige(snow.acc~1,test2,newdata=grid2,model=vgm.fit,nmax=20)
#image(test3, xlim=c(-160, -100), ylim=c(40, 85),asp=1.5,zlim=c(1,20),col=rev(brewer.pal(9,"Spectral")))
#contour(test3, add=T, lwd=0.5,xlim=c(-160, -100), ylim=c(40, 80),levels=c(0,5,10,15,20))
#map('world',add=T,lwd=0.5,xlim=c(-160, -100), ylim=c(40, 80))
#axis(side=1)
#axis(side=2)
#rm(grid,plot.data)
gc()

# Load additional shapefiles
canada_borders<-readOGR(dsn="shapefiles/canadaborders.shp",layer="canadaborders")
canada_borders <- spTransform(canada_borders, CRS("+proj=latlong"))
canada_borders <- as(canada_borders, "SpatialLinesDataFrame")
roads<-readOGR(dsn="shapefiles/roads.shp",layer="roads")
roads <- spTransform(roads, CRS("+proj=latlong"))
roads <- as(roads, "SpatialLinesDataFrame")
usa_borders<-readOGR(dsn="shapefiles/usa.shp",layer="usa")
#usa_borders <- spTransform(usa_borders, CRS("+proj=latlong"))
usa_borders <- as(usa_borders, "SpatialLinesDataFrame")

# Add annotations for major cities / towns
#cities=data.frame(c("Calgary",-114.078225,51.05456),
#                  c("Vancouver",-123.113927,49.261226),
#                  c("Edmonton",-113.5006,53.5472)
#)
rownames(poi.cities)<-c("City","x","y")
colnames(poi.cities)<-NULL
poi.cities<-data.frame(t(poi.cities))
poi.cities$x=as.numeric(as.character(poi.cities$x))
poi.cities$y=as.numeric(as.character(poi.cities$y))
coordinates(poi.cities)<-c('x','y')
proj4string(poi.cities)='+proj=longlat +ellps=GRS80 +towgs84'
poi.cities<-spTransform(poi.cities,CRS('+proj=latlong'))

# Add annotations for resorts
rownames(poi.resorts)<-c("Resort","x","y")
colnames(poi.resorts)<-NULL
poi.resorts<-data.frame(t(poi.resorts))
poi.resorts$x=as.numeric(as.character(poi.resorts$x))
poi.resorts$y=as.numeric(as.character(poi.resorts$y))
#write.csv(resorts,file="data/resorts.csv",row.names=FALSE)
coordinates(poi.resorts)<-c('x','y')
proj4string(poi.resorts)='+proj=longlat +ellps=GRS80 +towgs84'
poi.resorts<-spTransform(poi.resorts,CRS('+proj=latlong'))

# Setup Plot
rgb.palette=colorRampPalette(rev(brewer.pal(n=9,name="Spectral")))
plotlim=data.frame(x=c(-132,-112),y=c(47,56))
coordinates(plotlim)<-c('x','y')
proj4string(plotlim)='+proj=longlat +ellps=GRS80 +towgs84'
plotlim<-spTransform(plotlim,CRS('+proj=latlong'))
rm(x,y,coords,flipVertical)

p1=spplot(test2,c("snow.acc"),names.attr=c("Snow Accumulation"),
          col.regions=colorRampPalette(c("white", "#C5E9FF", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598","#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F", "#9E0142")),
          at=c(0,1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80),
          colorkey=list(labels=list(at=c(1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80),labels=c(1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80))),
          main=paste0("5-day Global GEM - Snowfall Forecast (cm) for ",date.fancy),
          xlab="Snow-Reports.ca - Forecast Data: Environment Canada (Regional RDPS), (Snow Dens.: ~100kg/m3) \n Map: GeoGratis © Department of Natural Resources Canada. All rights reserved.",
          scales=list(draw=FALSE),
          xlim=c(plotlim@bbox[1],plotlim@bbox[3]),
          ylim=c(plotlim@bbox[2],plotlim@bbox[4]),
          sp.layout=list(c('sp.points',poi.cities,col="black",pch=21,cex=0.8,lwd=1,fill="grey"),
                         c('sp.lines',canada_borders,col="darkgrey",lwd=2),
                         c('sp.lines',usa_borders,col="darkgrey",lwd=2,lty=2),
                         c('sp.points',poi.resorts,col="black",pch=15,cex=0.5),
                         c('sp.lines',roads,col="grey",lwd=1,alpha=0.5),
                         do.call("list",list("sp.text",coordinates(poi.resorts),poi.resorts$Resort,pos=4,cex=0.7))
          ),
          contour=FALSE,
          labels=list(
            labels=c(1,2,5,10,15,20,25,30,35,40,45,50,60,70,80),
            col="black",
            cex="0.75"
          )
)
#gc()
#png(filename="graphs/naefs_tmp.png",width=700,height=550,units="px",res=72,type="cairo")
print(p1)
#dev.off()

# Update live version on server
system("mv graphs/naefs_tmp.png graphs/naefs.png")
