# Modular Code to generate HRDPS Precipitation Maps

plot.HRDPS.precipitation<-function(type="snow",output=FALSE,file.suffix="",width=700,height=465,res=72)
{
  var=switch(type,
             rain = "RPRATE_surface",
             snow = "SPRATE_surface")
  
  title=switch(var,
               SPRATE_surface = "24h Forecast Snowfall (cm)",
               RPRATE_surface = "24h Forecast Rain (mm)"
  )
  filename=NULL
  if(output){
  filename=switch(var,
              SPRATE_surface = "24h_forecast_snow",
              RPRATE_surface = "24h_forecast_rain",
    )
  }
  
  
  model0 <- nc_open("cache/ec_hrdps/ec_hrdps_000.nc")
  model <- nc_open("cache/ec_hrdps/ec_hrdps.nc",readunlim=FALSE)
  date.fancy=format(as.POSIXct.numeric(min(ncvar_get(model0,"time")),tz="GMT",origin="1970-01-01"),"%Y-%m-%d %H:%M %Z",tz="America/Edmonton")
  x <- as.vector(ncvar_get(model, "longitude"))-360
  y <- as.vector(ncvar_get(model, "latitude"))
  
  variable <- ncvar_get(model, var)[,,]
  variable=aperm(variable,c(1,2,3))
  variable=as.vector(apply(variable,MARGIN=c(1,2),FUN=max))
  
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
  
  # Generate Plot
  p<-spplot(test2,c("variable"),names.attr=c("Forecast Precipitation"),
            col.regions=colorRampPalette(c("white", "#C5E9FF", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598","#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F", "#9E0142")),
            at=c(0,1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80,100),
            colorkey=list(labels=list(at=c(1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80,100),labels=c(1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80,100))),
            main=paste0(title, " for ",date.fancy),
            xlab="Snow-Reports.ca - Forecast Data: Environment Canada (HRDPS West), (Snow Density: 100kg/m3) \n Map: GeoGratis © Department of Natural Resources Canada. All rights reserved.",
            scales=list(draw=FALSE),
            contour=FALSE,
            xlim=c(plotlim@bbox[1],plotlim@bbox[3]),
            ylim=c(plotlim@bbox[2],plotlim@bbox[4]),
            sp.layout=splayoutList
  )
  if(!is.null(filename))
  {
    dev<-png(filename=paste0("tmp/",filename,"_tmp.png"),width=width,height=height,type="cairo",res=res)
    print(p)
    dev.off()
    system(paste0("mv tmp/",filename,"_tmp.png graphs/",filename,file.suffix,".png"))
    cat(paste0("[Plot for ",title," saved to: graphs/",filename,file.suffix,".png]\r\n"))
  } else {
    print(p)
  }
}

animate.HRDPS.snow<-function()
{
  var = "SPRATE_surface"
  title ="24h Forecast Cumulative Snowfall"
  filename = "24h_forecast_snow_animation"
  
  model0 <- nc_open("cache/ec_hrdps/ec_hrdps_000.nc")
  model <- nc_open("cache/ec_hrdps/ec_hrdps.nc",readunlim=FALSE)
  date.fancy=format(as.POSIXct.numeric(min(ncvar_get(model0,"time")),tz="GMT",origin="1970-01-01"),"%Y-%m-%d %H:%M %Z",tz="America/Edmonton")
  datestamps<-format(as.POSIXct.numeric(ncvar_get(model,"time"),tz="GMT",origin="1970-01-01"),"%Y-%m-%d %H:%M %Z",tz="America/Edmonton")
  x <- as.vector(ncvar_get(model, "longitude"))-360
  y <- as.vector(ncvar_get(model, "latitude"))
  
  variable <- ncvar_get(model, var)[,,]
  variable=aperm(variable,c(1,2,3))
  #variable=as.vector(apply(variable,MARGIN=c(1,2),FUN=mean))
  
  plot.data<<-variable
  suppressWarnings(rm(variable,model,model0))
  
  gc()
  coords=data.frame(long=x,lat=y)
  coordinates(coords)=~long+lat
  proj4string(coords)<-CRS("+proj=longlat")
  grid=SpatialGrid(GridTopology(bbox(coords)[,1],c(2500,2500),c(685,485)))
  #test=SpatialPointsDataFrame(coords=coords,data=plot.data,
  #                            proj4string=CRS("+proj=latlong"))
  #test.merc <- spTransform(test,CRS(proj.HRDPS))
  #grid<-points2grid(test.merc)
  #test2<-SpatialGridDataFrame(grid,plot.data)
  #rm(grid,test,test.merc)
  #gc()
  #test2=flipVertical(test2)
  
  loadShapefiles(crs=proj.HRDPS)
  
  # Add annotations for major cities / towns & Resorts
  cities<-spTransform(poi.cities,CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=247 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs'))
  resorts<-spTransform(poi.resorts,CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=247 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs'))
  
  rgb.palette=colorRampPalette(rev(brewer.pal(n=9,name="Spectral")))
  plotlim=data.frame(x=c(-128,-112),y=c(47,56.5))
  coordinates(plotlim)<-c('x','y')
  proj4string(plotlim)='+proj=longlat +ellps=GRS80 +towgs84'
  plotlim<-spTransform(plotlim,CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=247 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs'))
  rm(x,y)
  
  splayoutList<-list(c('sp.points',cities,col="black",pch=21,cex=0.8,lwd=1,fill="grey"),
                     c('sp.lines',shpCanBorders,col="darkgrey",lwd=2),
                     c('sp.points',resorts,col="black",pch=15,cex=0.5),
                     c('sp.lines',shpCanRoads,col="grey",lwd=1,alpha=0.5),
                     c('sp.lines',shpUSBorders,col="grey",lwd=1.25),
                     do.call("list",list("sp.text",coordinates(resorts),resorts$Resort,pos=4,cex=0.65))
  )
  
  # Generate Plot
  for(i in 1:dim(plot.data)[3])
  {
    file.no<-formatC(i,width=2,flag="0")
    png(file=paste0("tmp/24h_forecast_snow_animate_tmp",file.no,".png"),width=640, height=480,type="cairo")
    ani.data<-data.frame(snow=as.vector(plot.data[,,i]))
    test=SpatialPointsDataFrame(coords=coords,data=ani.data,
                                proj4string=CRS("+proj=latlong"))
    test.merc <- spTransform(test,CRS(proj.HRDPS))
    grid<-points2grid(test.merc)
    test2<-SpatialGridDataFrame(grid,ani.data)
    rm(grid,test,test.merc)
    gc()
    test2=flipVertical(test2)
    p<-spplot(test2,c("snow"),names.attr=c("Forecast Snow"),
              col.regions=colorRampPalette(c("white", "#C5E9FF", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598","#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F", "#9E0142")),
              at=c(0,1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80,100),
              colorkey=list(labels=list(at=c(1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80,100),labels=c(1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80,100))),
              main=paste0(title, " for ",datestamps[i]),
              xlab="Snow-Reports.ca - Forecast Data: Environment Canada (HRDPS West), (Snow Density 100kg/m3)\n Map: GeoGratis © Department of Natural Resources Canada. All rights reserved.",
              scales=list(draw=FALSE),
              contour=FALSE,
              xlim=c(plotlim@bbox[1],plotlim@bbox[3]),
              ylim=c(plotlim@bbox[2],plotlim@bbox[4]),
              sp.layout=splayoutList
    )
    print(p)
    dev.off()
  }
  system("convert -delay 35 -loop 50 tmp/24h_forecast_snow_animate_tmp*.png tmp/24h_forecast_snow_animation_tmp.gif")
  file.rename(from="tmp/24h_forecast_snow_animation_tmp.gif",to="graphs/24h_forecast_snow_animation.gif")
  #system("mv tmp/24h_forecast_snow_animation_tmp.gif graphs/24h_forecast_snow_animation.gif")
  #system("rm tmp/24h_forecast_snow_animate_tmp*.png")
  
}

animate.HRDPS.rain<-function()
{
  var = "RPRATE_surface"
  title ="24h Forecast Cumulative Rainfall"
  filename = "24h_forecast_rain_animation"
  
  model0 <- nc_open("cache/ec_hrdps/ec_hrdps_000.nc")
  model <- nc_open("cache/ec_hrdps/ec_hrdps.nc",readunlim=FALSE)
  date.fancy=format(as.POSIXct.numeric(min(ncvar_get(model0,"time")),tz="GMT",origin="1970-01-01"),"%Y-%m-%d %H:%M %Z",tz="America/Edmonton")
  datestamps<-format(as.POSIXct.numeric(ncvar_get(model,"time"),tz="GMT",origin="1970-01-01"),"%Y-%m-%d %H:%M %Z",tz="America/Edmonton")
  x <- as.vector(ncvar_get(model, "longitude"))-360
  y <- as.vector(ncvar_get(model, "latitude"))
  
  variable <- ncvar_get(model, var)[,,]
  variable=aperm(variable,c(1,2,3))
  #variable=as.vector(apply(variable,MARGIN=c(1,2),FUN=mean))

  plot.data<<-variable
  suppressWarnings(rm(variable,model,model0))
  
  gc()
  coords=data.frame(long=x,lat=y)
  coordinates(coords)=~long+lat
  proj4string(coords)<-CRS("+proj=longlat")
  grid=SpatialGrid(GridTopology(bbox(coords)[,1],c(2500,2500),c(685,485)))
  #test=SpatialPointsDataFrame(coords=coords,data=plot.data,
  #                            proj4string=CRS("+proj=latlong"))
  #test.merc <- spTransform(test,CRS(proj.HRDPS))
  #grid<-points2grid(test.merc)
  #test2<-SpatialGridDataFrame(grid,plot.data)
  #rm(grid,test,test.merc)
  #gc()
  #test2=flipVertical(test2)
  
  loadShapefiles(crs=proj.HRDPS)
  
  # Add annotations for major cities / towns & Resorts
  cities<-spTransform(poi.cities,CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=247 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs'))
  resorts<-spTransform(poi.resorts,CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=247 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs'))
  
  rgb.palette=colorRampPalette(rev(brewer.pal(n=9,name="Spectral")))
  plotlim=data.frame(x=c(-128,-112),y=c(47,56.5))
  coordinates(plotlim)<-c('x','y')
  proj4string(plotlim)='+proj=longlat +ellps=GRS80 +towgs84'
  plotlim<-spTransform(plotlim,CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=247 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs'))
  rm(x,y)
  
  splayoutList<-list(c('sp.points',cities,col="black",pch=21,cex=0.8,lwd=1,fill="grey"),
                     c('sp.lines',shpCanBorders,col="darkgrey",lwd=2),
                     c('sp.points',resorts,col="black",pch=15,cex=0.5),
                     c('sp.lines',shpCanRoads,col="grey",lwd=1,alpha=0.5),
                     c('sp.lines',shpUSBorders,col="grey",lwd=1.25),
                     do.call("list",list("sp.text",coordinates(resorts),resorts$Resort,pos=4,cex=0.65))
  )
  
  # Generate Plot
  for(i in 1:dim(plot.data)[3])
  {
    file.no<-formatC(i,width=2,flag="0")
    png(file=paste0("tmp/24h_forecast_rain_animate_tmp",file.no,".png"),width=640, height=480,type="cairo")
    ani.data<-data.frame(rain=as.vector(plot.data[,,i]))
    test=SpatialPointsDataFrame(coords=coords,data=ani.data,
                                proj4string=CRS("+proj=latlong"))
    test.merc <- spTransform(test,CRS(proj.HRDPS))
    grid<-points2grid(test.merc)
    test2<-SpatialGridDataFrame(grid,ani.data)
    rm(grid,test,test.merc)
    gc()
    test2=flipVertical(test2)
    p<-spplot(test2,c("rain"),names.attr=c("Forecast rain"),
              col.regions=colorRampPalette(c("white", "#C5E9FF", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598","#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F", "#9E0142")),
              at=c(0,1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80,100),
              colorkey=list(labels=list(at=c(1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80,100),labels=c(1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80,100))),
              main=paste0(title, " for ",datestamps[i]),
              xlab="rain-Reports.ca - Forecast Data: Environment Canada (HRDPS West)\n Map: GeoGratis © Department of Natural Resources Canada. All rights reserved.",
              scales=list(draw=FALSE),
              contour=FALSE,
              xlim=c(plotlim@bbox[1],plotlim@bbox[3]),
              ylim=c(plotlim@bbox[2],plotlim@bbox[4]),
              sp.layout=splayoutList
    )
    print(p)
    dev.off()
  }
  system("convert -delay 35 -loop 50 tmp/24h_forecast_rain_animate_tmp*.png tmp/24h_forecast_rain_animation_tmp.gif")
  system("mv tmp/24h_forecast_rain_animation_tmp.gif graphs/24h_forecast_rain_animation.gif")
  #system("rm tmp/24h_forecast_rain_animate_tmp*.png")
  
}