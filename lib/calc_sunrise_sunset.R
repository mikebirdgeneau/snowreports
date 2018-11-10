fetch.sunsriset<-function(poi="Lake Louise",type="resorts",dateTime=Sys.time())
{
  dateTime<-as.POSIXct(dateTime,tz="America/Edmonton")
  local.cells<-getCells.GEMRegional(type=type)
  local<-local.cells[which(local.cells$resort==poi),]
  suppressPackageStartupMessages(require(maptools))
  local.sunrise<-sunriset(matrix(c(local$x,local$y),nrow=1),dateTime=dateTime,direction=c("sunrise"),POSIXct.out=TRUE)$time
  local.sunset<-sunriset(matrix(c(local$x,local$y),nrow=1),dateTime=dateTime,direction=c("sunset"),POSIXct.out=TRUE)$time
  return(list(sunrise=local.sunrise,sunset=local.sunset))
}
