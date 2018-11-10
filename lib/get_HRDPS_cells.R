# Get GEM Regional Cells for POI

getCells.HRDPS<-function(type="resorts")
{
  df.poi<-switch(type,
                 resorts=poi.resorts,
                 backcountry=poi.backcountry,
                 helicat=poi.helicat,
                 cities=poi.cities,
                 towns=poi.towns
  )
  model0 <- nc_open("cache/ec_hrdps/ec_hrdps_000.nc")
  model <- nc_open("cache/ec_hrdps/ec_hrdps.nc",readunlim=FALSE)
  #dims=dim(ncvar_get(model0,"LAND_surface"))
  date.fancy=format(as.POSIXct.numeric(min(ncvar_get(model0,"time")),tz="UTC",origin="1970-01-01"),"%Y-%m-%d %H:%M %Z",tz="America/Edmonton")
  
  
  proj4string(df.poi)='+proj=longlat +ellps=GRS80 +towgs84'
  
  # Set-up Grid Cells
  x <- as.vector(ncvar_get(model, "longitude"))-360
  y <- as.vector(ncvar_get(model, "latitude"))
  coords=data.frame(x=x,y=y)
  coordinates(coords)=~x+y
  proj4string(coords)<-CRS("+proj=longlat")
  grid=SpatialGrid(GridTopology(bbox(coords)[,1],c(2500,2500),c(685,485)))
  rm(x,y)
  
  # Find grid cells closest to resorts.
  poi.cells<-data.frame(resort=df.poi@data$Resort,x=df.poi@coords[,1],y=df.poi@coords[,2])
  poi.cells$cellno<-NA
  rm(df.poi)
  for(i in 1:nrow(poi.cells)){
    if(min(sqrt(abs(coords@coords[,1]-poi.cells$x[i])^2+abs(coords@coords[,2]-poi.cells$y[i])^2))>0.03){
      poi.cells$cellno[i] <- NA
    } else {
      poi.cells$cellno[i]<-which.min(sqrt(abs(coords@coords[,1]-poi.cells$x[i])^2+abs(coords@coords[,2]-poi.cells$y[i])^2))
    }
  }
  poi.cells$cell.x<-coords@coords[poi.cells$cellno,1]
  poi.cells$cell.y<-coords@coords[poi.cells$cellno,2]
  
  #Convert to Row / Column Index
  poi.cells$row<-ceiling(poi.cells$cellno/685)
  poi.cells$col<-685-(poi.cells$row*685-poi.cells$cellno)
  
  poi.cells <- poi.cells[which(is.finite(poi.cells$cellno)),]
  
  return(poi.cells)
}