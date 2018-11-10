require(RMySQL)
require(data.table)

get.date<-Sys.Date()
if(format(Sys.time(),format="%H")>17)
{
  suffix<-"00:00:00"
} else {
  suffix<-"12:00:00"
}

# Get Data for today
con<-dbConnect(MySQL(),user="snowreports",password="dbpass",dbname="snowfall",host="localhost")
sql<-paste0("select * from ecforecast24 where dmin like '",get.date," ",suffix,"'")
dt.fcst<-data.table(dbGetQuery(con,sql))

save(dt.fcst, file="dt.fcst.Rda")

# Close connection
dbDisconnect(con)
