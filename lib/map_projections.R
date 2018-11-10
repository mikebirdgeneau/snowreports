# Set-up Projections for Data, to allow for plotitng of regular grids

proj.HRDPS<-"+proj=stere +lat_0=90 +lat_ts=60 +lon_0=247 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs"

proj.GEMRegional<-"+proj=stere +lat_0=90 +lat_ts=60 +lon_0=249 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs"

proj.GEMGlobal<-"+proj=longlat +ellps=GRS80 +towgs84" # +pm=-181
