# Load Shapefiles for mapping, using desired CRS if needed.
loadShapefiles<-function(crs="+proj=latlong")
{
  # Canadian Borders
  shpCanBorders<<-readOGR(dsn="shapefiles/canadaborders.shp",layer="canadaborders",verbose=FALSE)
  shpCanBorders<<-spTransform(shpCanBorders, CRS(crs))
  shpCanBorders<<- as(shpCanBorders, "SpatialLinesDataFrame")
  # Canadian Roads
  shpCanRoads <<-readOGR(dsn="shapefiles/roads.shp",layer="roads",verbose=FALSE)
  shpCanRoads <<- spTransform(shpCanRoads, CRS(crs))
  shpCanRoads <<- as(shpCanRoads, "SpatialLinesDataFrame")
  # US Borders
  shpUSBorders <<-readOGR(dsn="shapefiles/usa.shp",layer="usa",verbose=FALSE)
  proj4string(shpUSBorders)<<-"+proj=longlat +ellps=GRS80 +towgs84"
  shpUSBorders <<- spTransform(shpUSBorders, CRS(crs))
  shpUSBorders <<- as(shpUSBorders, "SpatialLinesDataFrame")
}