# Save regional fcst to SQL to test performance and possibility for new functionality...
library(ncdf)
library(data.table)
library(RMySQL)

model0 <- nc_open("cache/ec_gemregional/ec_gemregional_000.nc")
model <- nc_open("cache/ec_gemregional/ec_gemregional.nc",readunlim=FALSE)
model.ext <- nc_open("cache/ec_gemregional/ec_gemregional_ext.nc",readunlim=FALSE)

#### Table of Points ####
# Get Latitude & Longitude
dt.regional.pts<-data.table(point_id=seq(1,length(as.vector(ncvar_get(model0, "latitude"))),by=1),longitude=as.vector(ncvar_get(model0, "longitude"))-360,latitude=as.vector(ncvar_get(model0, "latitude")))

con<-dbConnect(dbDriver("MySQL"), host="localhost",user="snowreports", pass="dbpass",db="meteo_ecregional")

# Create Table for Points
if(!dbExistsTable(con, "points"))
{
  col.info<-"(point_id int NOT NULL PRIMARY KEY, longitude double, latitude double)"
  dbSendQuery(con, paste("CREATE TABLE IF NOT EXISTS", "points", col.info, sep=" ")); 
}

# Populate table with points
if(dbExistsTable(con, "points"))
{
  dbWriteTable(con, "points", dt.regional.pts, row.names=FALSE, append=TRUE);
}

#### Table w Data ####
# Create Table for Points
if(!dbExistsTable(con, "forecast"))
{
  col.info<-"(point_id int NOT NULL, fcst_issue TIMESTAMP NULL DEFAULT NULL, fcst_time TIMESTAMP NULL DEFAULT NULL, variable VARCHAR(30), value DOUBLE,CONSTRAINT pk_forecastID PRIMARY KEY (point_id,fcst_issue,fcst_time,variable))"
  dbSendQuery(con, paste("CREATE TABLE IF NOT EXISTS", "forecast", col.info, sep=" ")) 
  #dbSendQuery(con,"ALTER TABLE forecast ADD CONSTRAINT uniqueFcst UNIQUE(point_id,fcst_issue,fcst_time,variable)")
  #dbSendQuery(con,"ALTER TABLE forecast ADD CONSTRAINT uniqueFcst PRIMARY KEY (point_id,fcst_issue,fcst_time,variable)")
}

fcst_issue<-format(as.POSIXct.numeric(ncvar_get(model0,varid = "time"),tz="UTC",origin="1970-01-01"),"%Y-%m-%d %H:%M:%S",tz="UTC")

# Temperature
dt.temp<-data.table(point_id=seq(1,length(as.vector(ncvar_get(model0, "latitude")))),fcst_issue=fcst_issue,fcst_time=rep(format(as.POSIXct.numeric(ncvar_get(model,"time"),tz="UTC",origin="1970-01-01"),"%Y-%m-%d %H:%M:%S",tz="UTC"),each=length(as.vector(ncvar_get(model0, "latitude")))),variable="TMP_2maboveground",value=aperm(ncvar_get(model, "TMP_2maboveground")[,,],c(1,2,3)))

pb<-txtProgressBar(min=0,max=20,style = 3)
for(i in 0:19){
  dbSendQuery(con,"ALTER TABLE forecast DISABLE KEYS")
  setTxtProgressBar(pb,value = i)
  start = i*nrow(dt.temp)/20+1
  finish = (i+1)*nrow(dt.temp)/20
  dbWriteTable(con,"forecast",dt.temp[start:finish,],row.names=FALSE,append=TRUE,field.types=list(point_id="INT",fcst_issue="TIMESTAMP",fcst_time="TIMESTAMP", variable = "VARCHAR(30)",value="DOUBLE"))
  dbSendQuery(con,"ALTER TABLE forecast ENABLE KEYS")
}

# Precipitation
dt.temp<-data.table(point_id=seq(1,length(as.vector(ncvar_get(model0, "latitude")))),fcst_issue=fcst_issue,fcst_time=rep(format(as.POSIXct.numeric(ncvar_get(model,"time"),tz="UTC",origin="1970-01-01"),"%Y-%m-%d %H:%M:%S",tz="UTC"),each=length(as.vector(ncvar_get(model0, "latitude")))),variable="TMP_2maboveground",value=aperm(ncvar_get(model, "TMP_2maboveground")[,,],c(1,2,3)))

pb<-txtProgressBar(min=0,max=20,style = 3)
for(i in 0:19){
  dbSendQuery(con,"ALTER TABLE forecast DISABLE KEYS")
  setTxtProgressBar(pb,value = i)
  start = i*nrow(dt.temp)/20+1
  finish = (i+1)*nrow(dt.temp)/20
  dbWriteTable(con,"forecast",dt.temp[start:finish,],row.names=FALSE,append=TRUE,field.types=list(point_id="INT",fcst_issue="TIMESTAMP",fcst_time="TIMESTAMP", variable = "VARCHAR(30)",value="DOUBLE"))
  dbSendQuery(con,"ALTER TABLE forecast ENABLE KEYS")
}


dbDisconnect(con)
