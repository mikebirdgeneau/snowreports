library(raster)
library(leaflet)
library(ncdf)
library(RColorBrewer)

model <- nc_open("cache/ec_hrdps/ec_hrdps.nc",readunlim=FALSE)
x <- as.vector(ncvar_get(model, "longitude"))-360
y <- as.vector(ncvar_get(model, "latitude"))
r <- brick("cache/ec_hrdps/ec_hrdps.nc",varname="TCDC_surface")

extent(r) <- c(range(x),range(y))
crs(r) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

pal <- colorNumeric(rev(brewer.pal(9,name="PuBu")), domain = c(1,100), na.color = "transparent")

leaflet() %>% addTiles() %>%
  addRasterImage(r[[24]], colors = pal, opacity = 0.8,layerId = "Cloud") #%>%
  #addLegend(pal = pal, values = values(r), title = "Legend",)

