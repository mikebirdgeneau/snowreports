# Get current date in UTC
date=format(as.POSIXct(Sys.time()-7*3600, tz=format(Sys.time(),"%Z")),"%Y%m%d",tz="UTC")
date.fancy=format(as.POSIXct(Sys.time()-7*3600, tz=format(Sys.time(),"%Z")),"%Y-%m-%d",tz="UTC")

# Prefix
prefix="http://dd.weatheroffice.gc.ca/ensemble/naefs/grib2/raw/00/"
suffix=".grib2"

# Set List of post-fix data to gather
fcst.list.0=c("HGT_SFC_0","LAND_SFC_0","SNOD_SFC_0","WTMP_SFC_0")
fcst.list.1=c("ASNOW_SFC_0",
              "TCDC_SFC_0",
              "TMP_TGL_2")

# Fetch 00 Hr Forecast
dl.list.0=paste0("*",fcst.list.0,"*",date,"*",suffix)
system(paste0("wget -q -nd -N -r -l1 -P 'naefs' -A ",paste(dl.list.0,collapse=",")," ",prefix,"000/"))
system(paste0("ls naefs/*",date,"*.grib2 | xargs cat > results/naefs_000-",date,".grib2"))
system("rm naefs/*.grib2")

# Fetch non zero forecasts
dl.list.1=paste0("*",fcst.list.1,"*",date,"*",suffix)
for(i in seq(6, 120, by=6)){
  if(i<10){
    system(paste0("wget -q -nd -N -r -l1 -P 'naefs' -A ",paste(dl.list.1,collapse=",")," ",prefix,"00",i,"/"))
  }
  if(i>=10){
    system(paste0("wget -q -nd -N -r -l1 -P 'naefs' -A ",paste(dl.list.1,collapse=",")," ",prefix,"0",i,"/"))    
  }
  if(i>=100){
    system(paste0("wget -q -nd -N -r -l1 -P 'naefs' -A ",paste(dl.list.1,collapse=",")," ",prefix,i,"/"))    
  }
}

# Combine into a single grib2 file:
system(paste0("ls naefs/*",date,"*.grib2 | xargs cat > results/naefs-",date,".grib2"))
system("rm naefs/*.grib2")
system(paste0("wgrib2 results/naefs_000-",date,".grib2 -netcdf results/naefs_000.nc"))
system(paste0("wgrib2 results/naefs-",date,".grib2 -netcdf results/naefs.nc"))

# Clean-up Env
rm(list=ls())
gc()
