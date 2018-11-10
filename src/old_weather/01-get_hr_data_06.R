# Get current date in UTC
date=format(as.POSIXct(Sys.time()-0*3600, tz=format(Sys.time(),"%Z")),"%Y%m%d",tz="UTC")
date.fancy=format(as.POSIXct(Sys.time()-0*3600, tz=format(Sys.time(),"%Z")),"%Y-%m-%d",tz="UTC")

# Prefix
prefix="http://dd.weatheroffice.gc.ca/model_hrdps/west/grib2/06/"
suffix=".grib2"

# Set List of post-fix data to gather
fcst.list.0=c("HGT_SFC_0","LAND_SFC_0","SNOD_SFC_0","WTMP_SFC_0")
fcst.list.1=c("HGT_SFC_0","PRATE_SFC_0","APCP_SFC_0","TCDC_SFC_0","TMP_TGL_2","WEASN_SFC_0",
              "WEARN_SFC_0","PRES_SFC_0","WIND_TGL_10","WDIR_TGL_10")

# Fetch 00 Hr Forecast
dl.list.0=paste0("*",fcst.list.0,"*",date,"*",suffix)
system(paste0("wget -q -nd -N -r -l1 -P 'hr_model_data' -A ",paste(dl.list.0,collapse=",")," ",prefix,"000/"))
system(paste0("ls hr_model_data/*",date,"*.grib2 | xargs cat > results/hrmodel_000-",date,".grib2"))
system("rm hr_model_data/*.grib2")

# Fetch non zero forecasts
dl.list.1=paste0("*",fcst.list.1,"*",date,"*",suffix)
for(i in 1:24){
  if(i<10){
    system(paste0("wget -q -nd -N -r -l1 -P 'hr_model_data' -A ",paste(dl.list.1,collapse=",")," ",prefix,"00",i,"/"))
  } else {
    system(paste0("wget -q -nd -N -r -l1 -P 'hr_model_data' -A ",paste(dl.list.1,collapse=",")," ",prefix,"0",i,"/"))    
  }
}

# Combine into a single grib2 file:
system(paste0("ls hr_model_data/*",date,"*.grib2 | xargs cat > results/hrmodel-",date,".grib2"))
system("rm hr_model_data/*.grib2")
system(paste0("wgrib2 results/hrmodel_000-",date,".grib2 -netcdf results/hrmodel_000.nc"))
system(paste0("wgrib2 results/hrmodel-",date,".grib2 -netcdf results/hrmodel.nc"))

# Clean-up Env
rm(list=ls())
gc()
