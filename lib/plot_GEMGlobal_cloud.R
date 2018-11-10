# Modular Code to generate HRDPS Precipitation Maps

plot.GEMGlobal.cloud<-function(output=TRUE,file.suffix="",width=1200,height=600,res=72,limit.hr=NA,min.hr=NA)
{
  graphics.off()
  if(is.na(limit.hr)){
    limit.hr=120
  }
  limit.hr=round(limit.hr/3,0)*3
  limit.day=round(limit.hr/24,0)
  if(is.na(min.hr))
  {
    min.hr.limit=1
  } else {
    min.hr.limit=round(min.hr/3,0)+1
  }
  var="TCDC_surface"
  
  title=paste0(limit.hr,"h, ",limit.hr-min.hr,"h mean Cloud Cover (%)")
  
  filename=NULL
  if(output){
    filename=paste0(ifelse(limit.hr<100,"0",""),limit.hr,"h_forecastGEMGlobal_cloud")
  }
  
  model0 <- nc_open(paste0(path,"cache/ec_gemglobal/ec_gemglobal_000.nc"))
  model <- nc_open(paste0(path,"cache/ec_gemglobal/ec_gemglobal.nc"),readunlim=FALSE)
  date.fancy=format(as.POSIXct.numeric(min(ncvar_get(model0,"time")),tz="GMT",origin="1970-01-01"),"%Y-%m-%d %H:%M %Z",tz="America/Edmonton")
  x <- as.vector(ncvar_get(model, "longitude",start=c(1),count=c(564)))
  #x[which(x<(-180))]<-360+x[which(x<(-180))]
  y <- as.vector(ncvar_get(model, "latitude",start=c(500),count=c(-1)))
  
  #ncvar_get(model,"longitude",start=c(940),count=c(-1))
  #ncvar_get(model,"latitude",start=c(526),count=c(-1))
  #ncvar_get(model,"time",start=c(1),count=c(-1))
  #message(paste0("[Min hr idx:",min.hr.limit," ]"))
  
  variable <- ncvar_get(model, var,start=c(1,500,min.hr.limit),count=c(564,-1,limit.hr/3-min.hr.limit))/100#[,,1:limit.hr/3]
  variable=aperm(variable,c(1,2,3))
  variable=as.vector(apply(variable,MARGIN=c(1,2),FUN=mean))
  
  plot.data<-data.table(variable=variable)
  suppressWarnings(rm(variable,model,model0))
  
  gc()
  coords=expand.grid(x=x,y=y)
  coordinates(coords)=~x+y
  proj4string(coords)<-CRS("+proj=longlat +ellps=GRS80 +towgs84")
  coords<-spTransform(coords,CRS(proj.GEMGlobal))
  grid=SpatialGrid(GridTopology(bbox(coords)[,1],c(0.24,0.24),c(length(x),length(y))))
  test2<-SpatialGridDataFrame(grid,plot.data)
  rm(grid,plot.data)
  gc()
  test2=flipVertical(test2)
  
  loadShapefiles(crs=proj.GEMGlobal)
  
  #shpCanBorders<<-readOGR(dsn="shapefiles/canaborders2.shp",layer="canaborders2",verbose=FALSE)
  #shpCanBorders<<-spTransform(shpCanBorders, CRS("+proj=longlat +ellps=GRS80 +towgs84 +over +lon_wrap=180"))
  #shpCanBorders<<- as(shpCanBorders, "SpatialLinesDataFrame")
  
  # Add annotations for major cities / towns & Resorts
  cities<-spTransform(poi.cities,CRS(proj.GEMGlobal))
  resorts<-spTransform(poi.resorts,CRS(proj.GEMGlobal))
  
  rgb.palette=colorRampPalette(rev(brewer.pal(n=9,name="Spectral")))
  plotlim=data.frame(x=c(-180,-45),y=c(30,90))
  coordinates(plotlim)<-c('x','y')
  proj4string(plotlim)=proj.GEMGlobal
  #plotlim<-spTransform(plotlim,CRS(proj.GEMGlobal))
  rm(x,y,coords)
  
  splayoutList<-list(c('sp.points',cities,col="black",pch=21,cex=0.8,lwd=1,fill="grey"),
                     c('sp.lines',shpCanBorders,col="darkgrey",lwd=2),
                     #c('sp.points',resorts,col="black",pch=15,cex=0.5),
                     #c('sp.lines',shpCanRoads,col="grey",lwd=1,alpha=0.5),
                     c('sp.lines',shpUSBorders,col="grey",lwd=1.25)#,
                     #do.call("list",list("sp.text",coordinates(resorts),resorts$Resort,pos=4,cex=0.65))
  )
  splayoutListsm<-list(c('sp.points',cities,col="black",pch=21,cex=0.8,lwd=1,fill="grey"),
                       c('sp.lines',shpCanBorders,col="darkgrey",lwd=2),
                       #c('sp.lines',shpCanRoads,col="grey",lwd=1,alpha=0.5),
                       c('sp.lines',shpUSBorders,col="grey",lwd=1.25)
  )
  
  # Generate Plot
  p<-spplot(test2,c("variable"),names.attr=c("Forecast Precipitation"),
            col.regions=colorRampPalette(rev(brewer.pal(9,name="PuBu"))),
            at=c(-0.01,seq(0,1,by=0.01),1.01),
            main=paste0(title, " at ",date.fancy),
            xlab="Snow-Reports.ca - Forecast Data: Environment Canada (25km GDPS), (mm Water Equiv.) \n Map: GeoGratis © Department of Natural Resources Canada. All rights reserved.",
            scales=list(draw=FALSE),
            contour=FALSE,
            xlim=c(plotlim@bbox[1],plotlim@bbox[3]),
            ylim=c(plotlim@bbox[2],plotlim@bbox[4]),
            sp.layout=splayoutList
  )
  
  if(!is.null(filename))
  {
    png(filename=paste0(path,"tmp/",filename,"_tmp.png"),width=width,height=height,type="cairo",res=res)
    print(p)
    dev.off()
    file.rename(paste0("tmp/",filename,"_tmp.png"),paste0("graphs/",filename,file.suffix,".png"))
    #system(paste0("mv tmp/",filename,"_tmp.png ",path,"graphs/",filename,file.suffix,".png"))
    cat(paste0("[Plot for ",title," saved to: graphs/",filename,file.suffix,".png]\r\n"))
  } else {
    print(p)
  }
  
  #p<-spplot(test2,c("variable"),names.attr=c("Forecast Precipitation"),
  #          col.regions=colorRampPalette(rev(brewer.pal(9,name="PuBu"))),
  #          at=c(-0.01,seq(0,1,by=0.01),1.01),
  #          scales=list(draw=FALSE),
  #          contour=FALSE,
  #          xlim=c(plotlim@bbox[1],plotlim@bbox[3]),
  #          ylim=c(plotlim@bbox[2],plotlim@bbox[4]),
  #          sp.layout=splayoutListsm
  #)
  
  if(!is.null(filename))
  {
    png(filename=paste0(path,"graphs/",filename,"_smpdf.png"),width=round(width/2,0),height=round(height/2,0),type="cairo",res=res)
    print(p)
    dev.off()
    #file.rename(paste0("mv tmp/",filename,"_tmp_smpdf.png"),paste0("graphs/",filename,file.suffix,"_smpdf.png"))
    #system(paste0("mv tmp/",filename,"_tmp_smpdf.png ",path,"graphs/",filename,file.suffix,"_smpdf.png"))
  }
}