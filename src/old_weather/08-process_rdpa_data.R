# Required Packages
require(ncdf)
require(mapproj)
require(sp)
require(rgdal)
require(gstat)
require(stringr)
require(data.table)
require(ggplot2)
require(RColorBrewer)
library(gridExtra)
require(RMySQL)

setwd("/home/snowreports/R/weather/")
cat("* Starting RDPA Update Process.\n")

# Get current date in UTC
date=format(as.POSIXct(Sys.time()-24*3600, tz=format(Sys.time(),"%Z")),"%Y%m%d",tz="UTC")
date.fancy=format(as.POSIXct(Sys.time()-24*3600, tz=format(Sys.time(),"%Z")),"%Y-%m-%d",tz="UTC")

# Prefix
prefix="http://dd.weatheroffice.gc.ca/analysis/precip/rdpa/grib2/polar_stereographic/24/"
suffix=".grib2"

fcst.list.1=c("RDPA_APCP-024_SFC_0")
dl.list=paste0("*",fcst.list.1,"*",date,"*",suffix)

system(paste0("wget -q -nd -N -r -l1 -P 'rdpa_model_data' -A ",paste(dl.list,collapse=",")," ",prefix))
system(paste0("ls rdpa_model_data/*",date,"*.grib2 | xargs cat > results/rdpamodel_24-",date,".grib2"))
system("rm rdpa_model_data/*.grib2")

system(paste0("wgrib2 results/rdpamodel_24-",date,".grib2 -netcdf results/rdpamodel.nc"))
rm(list=ls())
gc()

cat("* Downloaded RDPA Data.\n")

# Get current date in UTC
date=format(as.POSIXct(Sys.time()-24*3600, tz=format(Sys.time(),"%Z")),"%Y%m%d",tz="UTC")

model <- nc_open("results/rdpamodel.nc",readunlim=FALSE)
date.fancy=format(as.POSIXct.numeric(min(ncvar_get(model,"time")),tz="UTC",origin="1970-01-01"),"%Y-%m-%d %H:%M %Z",tz="America/Edmonton")

# Add annotations for resorts
resorts<-data.frame(c("Apex",-119.901258,49.383206),
                    c("Big White",-118.936465,49.718226),
                    c("Castle",-114.414854,49.318039),
                    c("Fernie",-115.088050,49.462611),
                    c("Fortress",-115.198479,50.824895),
                    c("Kicking Horse",-117.078433,51.274922),
                    c("Kimberley",-116.016485,49.682298),
                    c("Lake Louise",-116.139285,51.466492),
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
                    c("Whitefish",-114.353560,48.486294),
                    c("Rogers Pass",-117.52000,51.300418),
                    c("Black Prince",-115.2367,50.706977),
                    c("Bow Summit",-116.498122,51.716055),
                    c("Highwood Pass",-114.967028,50.594155),
                    c("Kootenay Pass",-117.040151,49.056480),
                    c("Canmore",-115.357831,51.085274),
                    c("Calgary",-114.081533,51.053464),
                    c("Banff",-115.570893,51.178401))

rownames(resorts)<-c("Resort","x","y")
colnames(resorts)<-NULL
resorts<-data.frame(t(resorts))
resorts$x=as.numeric(as.character(resorts$x))
resorts$y=as.numeric(as.character(resorts$y))
coordinates(resorts)<-c('x','y')
proj4string(resorts)='+proj=longlat +ellps=GRS80 +towgs84'

# Set-up Grid Cells
x <- as.vector(ncvar_get(model, "longitude"))-360
y <- as.vector(ncvar_get(model, "latitude"))
coords=data.frame(x=x,y=y)
coordinates(coords)=~x+y
proj4string(coords)<-CRS("+proj=latlong")
grid=SpatialGrid(GridTopology(bbox(coords)[,1],c(10000,10000),c(935,824)))
rm(x,y)
# Find grid cells closest to resorts.
resort.cells<-data.frame(resort=resorts@data$Resort,x=resorts@coords[,1],y=resorts@coords[,2])
resort.cells$cellno<-NA
rm(resorts)
for(i in 1:nrow(resort.cells)){
  resort.cells$cellno[i]<-which.min(sqrt(abs(coords@coords[,1]-resort.cells$x[i])^2+abs(coords@coords[,2]-resort.cells$y[i])^2))
}
resort.cells$cell.x<-coords@coords[resort.cells$cellno,1]
resort.cells$cell.y<-coords@coords[resort.cells$cellno,2]

#Convert to Row / Column Index
resort.cells$row<-ceiling(resort.cells$cellno/935)
resort.cells$col<-935-(resort.cells$row*935-resort.cells$cellno)

# Get data by resort, over 24 hr period.
plot.data<-data.frame()
for(i in 1:nrow(resort.cells)){
  tmp.date<-as.POSIXct(format(as.POSIXct.numeric(ncvar_get(model,"time"),tz="UTC",origin="1970-01-01"),tz="America/Edmonton"))
  tmp.apcp<-ncvar_get(model,"APCP_surface",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,1))
  tmp.apcp.ci<-ncvar_get(model,"var0_1_193_surface",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,1))
  tmp<-data.frame(
    resort=resort.cells$resort[i],
    date=tmp.date,
    apcp=tmp.apcp,
    ci=tmp.apcp.ci)
  rm(tmp.date,tmp.apcp,tmp.apcp.ci)
  ifelse(i==1,plot.data<-tmp,plot.data<-rbind(plot.data,tmp))
}
plot.data<-data.table(plot.data)
setkeyv(plot.data,c("resort","date"))
#plot.data[,hrs:=(date-min(date))/3600+1,by=c("resort")]
rm(coords,grid,i,tmp)#resort.cells.x,resort.cells.y)
gc()

# Generate summary table for storing in database
filedate=format(min(plot.data$date),"%Y%m%d-%H",tz="America/Edmonton")
write.csv(plot.data,file=paste0("output/",filedate,"-rdpa_data.csv"),na="",row.names=FALSE)

# Write rows to DB
con<-dbConnect(MySQL(),user="snowreports",password="dbpass",dbname="snowfall",host="localhost")
on.exit(dbDisconnect(con))

for(i in 1:nrow(plot.data)){
sql<-sprintf("insert into rdpa (id,resort,`date`,apcp,ci) values (null,'%s','%s',%f,%f);",plot.data$resort[i],plot.data$date[i],plot.data$apcp[i],plot.data$ci[i]) 
if(i==1){print(paste0(sql,"\n"))}
rs<-dbSendQuery(con,sql)  
}
cat("* Done storing RDPA results to database.\n")

rm(list=ls())
gc()

#############################################################################
### Plot
#############################################################################

flipVertical <- function(x) {
  if (!inherits(x, "SpatialGridDataFrame")) stop("x must be a SpatialGridDataFrame")
  grd <- getGridTopology(x)
  idx = 1:prod(grd@cells.dim[1:2])
  m = matrix(idx, grd@cells.dim[2], grd@cells.dim[1], byrow = TRUE)[grd@cells.dim[2]:1, ]
  idx = as.vector(t(m)) 
  x@data <- x@data[idx, TRUE, drop = FALSE]
  x
}

model <- nc_open("results/rdpamodel.nc",readunlim=FALSE)
date.fancy=format(as.POSIXct.numeric(min(ncvar_get(model,"time")),tz="UTC",origin="1970-01-01"),"%Y-%m-%d %H:%M %Z",tz="America/Edmonton")

x <- as.vector(ncvar_get(model, "longitude"))-360
y <- as.vector(ncvar_get(model, "latitude"))
coords=data.frame(x=x,y=y)
coordinates(coords)=~x+y
proj4string(coords)<-CRS("+proj=latlong +datum=WGS84 +no_defs")
coords<-spTransform(coords,CRS("+proj=stere +lat_0=90 +lat_ts=60 +lon_0=249 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs"))
grid=SpatialGrid(GridTopology(bbox(coords)[,1],c(10000,10000),c(935,824)))

apcp <- ncvar_get(model, "APCP_surface")[,]
apcp=as.vector(aperm(apcp,c(1,2)))
plot.data<-data.table(apcp=apcp)
rm(apcp,model)
gc()

test2<-SpatialGridDataFrame(grid,plot.data)
rm(grid,plot.data,x,y)
gc()
test2=flipVertical(test2)
rm(flipVertical)

# Setup Plot
plotlim=data.frame(x=c(-128,-112),y=c(47,56.5))
coordinates(plotlim)<-c('x','y')
proj4string(plotlim)='+proj=longlat +ellps=GRS80 +towgs84'
plotlim<-spTransform(plotlim,CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=249 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs'))

# Load additional shapefiles
canada_borders<-readOGR(dsn="shapefiles/canadaborders.shp",layer="canadaborders")
canada_borders <- spTransform(canada_borders, CRS("+proj=stere +lat_0=90 +lat_ts=60 +lon_0=249 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs"))
canada_borders <- as(canada_borders, "SpatialLinesDataFrame")
roads<-readOGR(dsn="shapefiles/roads.shp",layer="roads")
roads <- spTransform(roads, CRS("+proj=stere +lat_0=90 +lat_ts=60 +lon_0=249 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs"))
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
cities<-spTransform(cities,CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=249 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs'))

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
resorts<-spTransform(resorts,CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=249 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs'))


p1.sm=spplot(test2,c("apcp"),names.attr=c("RDPA"),
          col.regions=colorRampPalette(c("white", "#C5E9FF", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598","#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F", "#9E0142")),
          at=c(0,1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80),
          colorkey=list(labels=list(at=c(1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80),labels=c(1,2.5,5,7.5,10,15,20,25,30,35,40,45,50,60,70,80))),             
             main=paste0("24h Precipitation Analysis (kg/m2) up to ",date.fancy),
             xlab="Snow-Reports.ca - Source: Environment Canada (RDPA), GeoGratis",
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
rm(canada_borders,cities,coords,plotlim,resorts,roads,test2)
gc()
#png(filename="graphs/24h_rdpa_sm.png",width=640,height=480,units="px",res=96,type="cairo")
#print(p1.sm)
#dev.off()
png(filename="graphs/24h_rdpa.png",width=700,height=525,units="px",res=72,type="cairo")
print(p1.sm)
dev.off()
rm(p1.sm)
gc()
