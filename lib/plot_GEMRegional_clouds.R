# Modular Code to generate HRDPS Precipitation Maps

plot.GEMRegional.clouds<-function(file.suffix="",width=700,height=465,res=72,limit.hr=NA)
{
  if(is.na(limit.hr)){
    limit.hr=48
  }
  limit.hr=round(limit.hr/3,0)*3
  
  var="TCDC_surface"
  title=paste0(limit.hr,"h Cloud Cover (%)")
  filename=paste0(limit.hr,"h_forecastGEM_cloud")
  
  model0 <- nc_open(paste0(path,"cache/ec_gemregional/ec_gemregional_000.nc"))
  model <- nc_open(paste0(path,"cache/ec_gemregional/ec_gemregional.nc"),readunlim=FALSE)
  date.fancy=format(as.POSIXct.numeric(min(ncvar_get(model0,"time")),tz="GMT",origin="1970-01-01"),"%Y-%m-%d %H:%M %Z",tz="America/Edmonton")
  x <- as.vector(ncvar_get(model, "longitude"))-360
  x[which(x<(-180))]<-360+x[which(x<(-180))]
  y <- as.vector(ncvar_get(model, "latitude"))
  
  variable <- ncvar_get(model, var)[,,1:limit.hr/3]/100
  variable=aperm(variable,c(1,2,3))
  variable=as.vector(apply(variable,MARGIN=c(1,2),FUN=max))
  
  plot.data<-data.table(variable=variable)
  suppressWarnings(rm(variable,model,model0))
  
  gc()
  coords=data.frame(x=x,y=y)
  coordinates(coords)=~x+y
  proj4string(coords)<-CRS("+proj=longlat")
  coords<-spTransform(coords,CRS(proj.GEMRegional))
  grid=SpatialGrid(GridTopology(bbox(coords)[,1],c(10000,10000),c(935,824)))
  test2<-SpatialGridDataFrame(grid,plot.data)
  rm(grid,plot.data)
  gc()
  test2=flipVertical(test2)
  
  loadShapefiles(crs=proj.GEMRegional)
  
  # Add annotations for major cities / towns & Resorts
  cities<-spTransform(poi.cities,CRS(proj.GEMRegional))
  resorts<-spTransform(poi.resorts,CRS(proj.GEMRegional))
  
  rgb.palette=colorRampPalette(rev(brewer.pal(n=9,name="Spectral")))
  plotlim=data.frame(x=c(-128,-112),y=c(47,56.5))
  coordinates(plotlim)<-c('x','y')
  proj4string(plotlim)='+proj=longlat +ellps=GRS80 +towgs84'
  plotlim<-spTransform(plotlim,CRS(proj.GEMRegional))
  rm(x,y,coords)
  
  splayoutList<-list(c('sp.points',cities,col="black",pch=21,cex=0.8,lwd=1,fill="grey"),
                     c('sp.lines',shpCanBorders,col="darkgrey",lwd=2),
                     c('sp.points',resorts,col="black",pch=15,cex=0.5),
                     c('sp.lines',shpCanRoads,col="grey",lwd=1,alpha=0.5),
                     c('sp.lines',shpUSBorders,col="grey",lwd=1.25),
                     do.call("list",list("sp.text",coordinates(resorts),resorts$Resort,pos=4,cex=0.65))
  )
  
  splayoutListsm<-list(c('sp.points',cities,col="black",pch=21,cex=0.8,lwd=1,fill="grey"),
                     c('sp.lines',shpCanBorders,col="darkgrey",lwd=2),
                     c('sp.lines',shpCanRoads,col="grey",lwd=1,alpha=0.5),
                     c('sp.lines',shpUSBorders,col="grey",lwd=1.25)
  )
  
  # Generate Plot
  p<-spplot(test2,c("variable"),names.attr=c("Forecast Precipitation"),
            col.regions=colorRampPalette(rev(brewer.pal(9,name="PuBu"))),
            at=c(-0.01,seq(0,1,by=0.01),1.01),
            main=paste0(title, " at ",date.fancy),
            xlab="Snow-Reports.ca - Forecast Data: Environment Canada (10km RDPS) \n Map: GeoGratis © Department of Natural Resources Canada. All rights reserved.",
            scales=list(draw=FALSE),
            contour=FALSE,
            xlim=c(plotlim@bbox[1],plotlim@bbox[3]),
            ylim=c(plotlim@bbox[2],plotlim@bbox[4]),
            sp.layout=splayoutList
  )

  if(!is.null(filename))
  {
    dev<-png(filename=paste0(path,"graphs/",filename,file.suffix,".png"),width=width,height=height,type="cairo",res=res)
    print(p)
    dev.off()
    #system(paste0("mv tmp/",filename,"_tmp.png ",path,"graphs/",filename,file.suffix,".png"))
    message(paste0("[Plot for ",title," saved to: graphs/",filename,file.suffix,".png]"))
  } else {
    print(p)
  }
  
  p<-spplot(test2,c("variable"),names.attr=c("Forecast Precipitation"),
             col.regions=colorRampPalette(rev(brewer.pal(9,name="PuBu"))),
             at=c(-0.01,seq(0,1,by=0.01),1.01),
             scales=list(draw=FALSE),
             contour=FALSE,
             xlim=c(plotlim@bbox[1],plotlim@bbox[3]),
             ylim=c(plotlim@bbox[2],plotlim@bbox[4]),
             sp.layout=splayoutListsm
  )
  
  if(!is.null(filename))
  {
    dev<-png(filename=paste0(path,"graphs/",filename,file.suffix,"_smpdf.png"),width=round(width/2,0),height=round(height/2,0),type="cairo",res=res)
    print(p)
    dev.off()
    #system(paste0("mv tmp/",filename,"_tmp_smpdf.png ",path,"graphs/",filename,file.suffix,"_smpdf.png"))
    message(paste0("[Plot for ",title," saved to: graphs/",filename,file.suffix,"_smpdf.png]"))
  }
  rm(p)
}