# Check current forecast dates, see if new ones are available!

# Check Global 25km GEM Model
checkNewFcst<-function(fcst="GEMGlobal")
{
  if(fcst=="GEMGlobal")
  {
    if(file.exists("cache/ec_gemglobal/ec_gemglobal_000.nc"))
    {
      current.fcst=as.POSIXct.numeric(min(ncvar_get(nc_open("cache/ec_gemglobal/ec_gemglobal_000.nc"),"time")),tz="GMT",origin="1970-01-01")
      attributes(current.fcst)$tzone="America/Edmonton"
      
    } else {
      current.fcst=as.POSIXlt(as.Date("1970-01-01"), tz="America/Edmonton")
      attributes(current.fcst)$tzone="America/Edmonton"
    }
    available.fcst<-getFcstTimeGlobal()
    available.fcst<-as.POSIXct(paste0(available.fcst[[2]]," ",available.fcst[[6]],":00"),tz="GMT")
    attributes(available.fcst)$tzone="America/Edmonton"

  } else if(fcst=="GEMRegional"){
    
    if(file.exists("cache/ec_gemregional/ec_gemregional_000.nc"))
    {
      current.fcst=as.POSIXct.numeric(min(ncvar_get(nc_open("cache/ec_gemregional/ec_gemregional_000.nc"),"time")),tz="GMT",origin="1970-01-01")
      attributes(current.fcst)$tzone="America/Edmonton"
    } else {
      current.fcst=as.POSIXlt(as.Date("1970-01-01"), tz="America/Edmonton")
      attributes(current.fcst)$tzone="America/Edmonton"
      }
    available.fcst<-getFcstTime()
    available.fcst<-as.POSIXct(paste0(available.fcst[[2]]," ",available.fcst[[6]],":00"),tz="GMT")
    attributes(available.fcst)$tzone="America/Edmonton"
  } else if(fcst=="HRDPS")
  {
    if(file.exists("cache/ec_hrdps/ec_hrdps_000.nc"))
    {
      current.fcst=as.POSIXct(min(ncvar_get(nc_open("cache/ec_hrdps/ec_hrdps_000.nc"),"time")),tz="GMT",origin="1970-01-01")
      attributes(current.fcst)$tzone="America/Edmonton"
    } else {
      current.fcst=as.POSIXlt(as.Date("1970-01-01"), tz="America/Edmonton")
      attributes(current.fcst)$tzone="America/Edmonton"
    }
    available.fcst<-getFcstTime()
    available.fcst<-as.POSIXct(paste0(available.fcst[[2]]," ",available.fcst[[6]],":00"),tz="GMT")
    attributes(available.fcst)$tzone="America/Edmonton"
  } else {
    warning(paste0("Forecast model ",fcst," not defined!"))
    return(list(fcst=fcst,current=NA,latest=NA,fetch=FALSE))
  }
  return(list(fcst=fcst,current=current.fcst,latest=available.fcst,fetch=available.fcst>current.fcst))
}
