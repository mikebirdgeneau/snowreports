library(ProjectTemplate)
reload.project()
library(raster)
library(leaflet)

var = "TCDC_surface"
title ="24h Forecast Mean Cloud Cover"
filename = "24h_forecast_cloud"

model0 <- nc_open("cache/ec_hrdps/ec_hrdps_000.nc")
model <- nc_open("cache/ec_hrdps/ec_hrdps.nc",readunlim=FALSE)
date.fancy=format(as.POSIXct.numeric(min(ncvar_get(model0,"time")),tz="GMT",origin="1970-01-01"),"%Y-%m-%d %H:%M %Z",tz="America/Edmonton")
x <- as.vector(ncvar_get(model, "longitude"))-360
y <- as.vector(ncvar_get(model, "latitude"))

variable <- ncvar_get(model, var)[,,]/100
variable=aperm(variable,c(1,2,3))
variable=as.vector(apply(variable,MARGIN=c(1,2),FUN=mean))

plot.data<-data.table(variable=variable)
suppressWarnings(rm(variable,model,model0))

gc()
coords=data.frame(long=x,lat=y)
coordinates(coords)=~long+lat
proj4string(coords)<-CRS("+proj=longlat")
grid=SpatialGrid(GridTopology(bbox(coords)[,1],c(2500,2500),c(685,485)))
test=SpatialPixelsDataFrame(data=plot.data,points = grid,proj4string=CRS(proj.HRDPS))
test.merc <- spTransform(test,CRS("+proj=longlat"))

library(raster)
test.rast <- raster(test.merc,layer="variable")

plot(test.rast)

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r),
                    na.color = "transparent")

leaflet() %>% addTiles() %>%
  addRasterImage(test.rast, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(r),
            title = "Surface temp")
