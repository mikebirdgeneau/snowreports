# Modular Code to generate HRDPS Temperature Map

plot.HRDPS.temperature<-function(fun="min",var="TMP_2maboveground",filename=NULL,width=700,height=465,res=72)
{
  title=switch(fun,
         min = "24h Forecast Low (C)",
         max = "24h Forecast High (C)",
         mean = "24h Mean Temperature (C)"
         )
  
  model0 <- nc_open("cache/ec_hrdps/ec_hrdps_000.nc")
  model <- nc_open("cache/ec_hrdps/ec_hrdps.nc",readunlim=FALSE)
  date.fancy=format(as.POSIXct.numeric(min(ncvar_get(model0,"time")),tz="GMT",origin="1970-01-01"),"%Y-%m-%d %H:%M %Z",tz="America/Edmonton")
  x <- as.vector(ncvar_get(model, "longitude"))-360
  y <- as.vector(ncvar_get(model, "latitude"))
  
  variable <- ncvar_get(model, var)[,,]
  variable=aperm(variable,c(1,2,3))
  val.adj=-273.15
  variable=as.vector(apply(variable,MARGIN=c(1,2),FUN=get(fun))+val.adj)

  plot.data<-data.table(variable=variable)
  suppressWarnings(rm(variable,model,model0))
  
  gc()
  coords=data.frame(long=x,lat=y)
  coordinates(coords)=~long+lat
  proj4string(coords)<-CRS("+proj=longlat")
  grid=SpatialGrid(GridTopology(bbox(coords)[,1],c(2500,2500),c(685,485)))
  test=SpatialPointsDataFrame(coords=coords,data=plot.data,
                              proj4string=CRS("+proj=latlong"))
  test.merc <- spTransform(test,CRS(proj.HRDPS))
  grid<-points2grid(test.merc)
  test2<-SpatialGridDataFrame(grid,plot.data)
  rm(grid,plot.data,test,test.merc)
  gc()
  test2=flipVertical(test2)

  loadShapefiles(crs=proj.HRDPS)
  
  # Add annotations for major cities / towns & Resorts
  cities<-spTransform(poi.cities,CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=247 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs'))
  resorts<-spTransform(poi.resorts,CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=247 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs'))
  
  rgb.palette=colorRampPalette(rev(brewer.pal(n=9,name="RdBu")))
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
  
  # Generate Plot
  p<-spplot(test2,c("variable"),names.attr=c("Forecast Low (°C)"),
         col.regions=rgb.palette,
         pretty=TRUE,
         contour=FALSE,
         #at=c(seq(floor(min(test2@data$variable)/5)*5,ceiling(max(test2@data$variable)/5)*5,by=2.5)),
          at=c(seq(-26.25,26.25,by=2.5)),
         main=paste0(title," for ",date.fancy),
         xlab="Snow-Reports.ca - Forecast Data: Environment Canada (HRDPS West)\n Map: GeoGratis © Department of Natural Resources Canada. All rights reserved.",
         scales=list(draw=FALSE),
         xlim=c(plotlim@bbox[1],plotlim@bbox[3]),
         ylim=c(plotlim@bbox[2],plotlim@bbox[4]),
         sp.layout=splayoutList
  )
  if(!is.null(filename))
  {
    dev<-png(filename=paste0("tmp/",filename,"_tmp.png"),width=width,height=height,type="cairo",res=res)
    print(p)
    dev.off()
    system(paste0("mv tmp/",filename,"_tmp.png graphs/",filename,".png"))
    cat(paste0("[Plot for ",title," saved to: graphs/",filename,".png]\r\n"))
  } else {
  print(p)
  }
}
