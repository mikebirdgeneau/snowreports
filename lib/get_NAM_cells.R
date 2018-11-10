# Get GEM Regional Cells for POI

getCells.NAM<-function(type="resorts")
{
  df.poi<-switch(type,
                 resorts=poi.resorts,
                 backcountry=poi.backcountry,
                 helicat=poi.helicat,
                 cities=poi.cities,
                 towns=poi.towns
  )
  model <- nc_open("cache/noaa_nam/noaa_nam.nc",readunlim=FALSE)
  date.fancy=format(as.POSIXct.numeric(min(ncvar_get(model,"time")),tz="UTC",origin="1970-01-01"),"%Y-%m-%d %H:%M %Z",tz="America/Edmonton")
  
  
  proj4string(df.poi)='+proj=longlat +ellps=GRS80 +towgs84'
  
  # Set-up Grid Cells
  x <- as.vector(ncvar_get(model, "longitude"))
  x[which(x>=180)]<-x[which(x>=180)]-360
  y <- as.vector(ncvar_get(model, "latitude"))
  coords=data.frame(x=x,y=y)
  coordinates(coords)=~x+y
  proj4string(coords)<-CRS("+proj=longlat")
  #grid=SpatialGrid(GridTopology(bbox(coords)[,1],c(12190.58,12190.58),cells.dim = c(262,177)))
  rm(x,y)
  
  # Find grid cells closest to resorts.
  poi.cells<-data.frame(resort=df.poi@data$Resort,x=df.poi@coords[,1],y=df.poi@coords[,2])
  poi.cells$cellno<-NA
  rm(df.poi)
  for(i in 1:nrow(poi.cells)){
    if(min(sqrt(abs(coords@coords[,1]-poi.cells$x[i])^2+abs(coords@coords[,2]-poi.cells$y[i])^2))>1.0)
    {
      poi.cells$cellno[i] <- NA
    } else {
      poi.cells$cellno[i] <- which.min(
        sqrt(abs(coords@coords[,1]-poi.cells$x[i])^2+abs(coords@coords[,2]-poi.cells$y[i])^2)
      )
    }
  }
  poi.cells$cell.x<-coords@coords[poi.cells$cellno,1]
  poi.cells$cell.y<-coords@coords[poi.cells$cellno,2]
  
  #Convert to Row / Column Index
  poi.cells$row<-ceiling(poi.cells$cellno/262)
  poi.cells$col<-262-(poi.cells$row*262-poi.cells$cellno)
  
  poi.cells <- poi.cells[which(is.finite(poi.cells$cellno)),]
  
  return(poi.cells)
}

