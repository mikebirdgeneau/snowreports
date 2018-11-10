# Setup Environment for Standalone Analysis
require(ProjectTemplate)
graphics.off()
reload.project()

if(checkNewFcst(fcst="HRDPS")$fetch==TRUE){
  fetchHRDPS()
}

# Save Fcst to Database


# Generate Map Images
plot.HRDPS.precipitation(type="snow",output=TRUE)
plot.HRDPS.temperature(fun="min",filename="24h_forecast_low.png")
plot.HRDPS.temperature(fun="max",filename="24h_forecast_high.png")
plot.HRDPS.cloud(output=TRUE)
plot.HRDPS.precipitation(type="rain",output=TRUE)

# Generate Resort Page Forecasts
cells.HRDPS <- getCells.HRDPS(type="resorts")
for(1 in 1:nrow(cells.HRDPS))
{
  local<-cells.HRDPS[1,]
}


cells.HRDPS <- getCells.HRDPS(type="backcountry")


cells.HRDPS <- rbind(getCells.HRDPS(type="cities"),getCells.HRDPS(type="towns"))


  
  

