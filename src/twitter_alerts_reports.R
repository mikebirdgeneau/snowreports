library(ProjectTemplate)
setwd("~/snow-reports.ca/")
suppressMessages(reload.project())

# setup_twitter_oauth(...) # Add API Key

con<-dbConnect(MySQL(),user="snowreports",password="<db password>",dbname="snowfall",host="localhost")
dt.snowreportdata<-data.table(dbGetQuery(con,"SELECT * FROM snowdatav2 WHERE id IN (SELECT MAX(id) FROM snowdatav2 GROUP BY resort)"))

dbDisconnect(con)
rm(con)

dt.snowreportdata <- dt.snowreportdata[as.Date(time)==Sys.Date(),]
setkeyv(dt.snowreportdata,c("snow24h","resort"))
dt.snowreportdata <- dt.snowreportdata[order(-snow24h),]
dt.snowreportdata <- dt.snowreportdata[snow24h>5,]
dt.snowreportdata[,text_snow:=paste0(resort,": ",snow24h,"cm"),]

if(nrow(dt.snowreportdata)>0){
  msg<-"24h Snow Reported:"
  i <- 1
  sentinel <- TRUE
  while(sentinel){
    if(i>nrow(dt.snowreportdata)){sentinel <- FALSE} else {
      if(str_length(paste0(msg,dt.snowreportdata$text_snow[i]))<140){
        msg<-paste0(msg,"\n",dt.snowreportdata$text_snow[i])
        i <- i + 1
      } else {
        sentinel <- FALSE
      }
    }
  }
  #print(msg)
  updateStatus(msg, displayCoords = FALSE)
}
