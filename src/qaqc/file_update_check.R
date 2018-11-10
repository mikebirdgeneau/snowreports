# Check for status of models & images
source("lib/pushoverr.R")

# Part 1: List files to check status on:
setwd("/home/snowreports/snow-reports.ca/")
dt.files<-data.frame(file=c(
  "cache/ec_gemregional/ec_gemregional_000.nc",
  "cache/ec_gemregional/ec_gemregional.nc",
  "cache/ec_hrdps/ec_hrdps_000.nc",
  "cache/ec_hrdps/ec_hrdps.nc",
  list.files("graphs",full.names=TRUE)[grepl("^(?!historical).*(\\.png|\\.gif)$",list.files("graphs",full.names=FALSE),perl=TRUE)]
  ))
dt.files$file<-as.character(dt.files$file)

# Part 2: Check File Modification Time and Check for files that require updates.

dt.files$mtime<-file.info(dt.files$file)$mtime
dt.files$chk<-ifelse(difftime(as.POSIXct(Sys.time()),dt.files$mtime,units="hours")>14,TRUE,FALSE)

if(nrow(dt.files[which(dt.files$chk),])>0)
{
  message("[Detected Files out of date; Sending Log.]")
  con<-file("log/file_status_check.txt","w+")
  cat(paste0(dt.files[which(dt.files$chk),]$mtime,"\t",dt.files[which(dt.files$chk),]$file,"\n"),file=con)
  system(paste0("sendEmail -f snowbot@snow-reports.ca -t mike@snow-reports.ca -u \"Snow-Reports.ca: Meteo Status Check Log for ",format(Sys.time(),"%Y-%m-%d %H:%M"),"\" -m \"Status Check found files not updated: \\n",paste0(dt.files[which(dt.files$chk),]$mtime,"\t",dt.files[which(dt.files$chk),]$file,"\n"),"\" -a /home/snowreports/snow-reports.ca/log/file_status_check.txt"))
  close(con)
  rm(con)
  pushover_high("Detected files that were not updated!")
  
} else {
  system("rm log/file_status_check.txt")
  system("touch log/file_status_check.txt")
  message("[No problems found. All files up-to-date.]")
  pushover("All Files up to Date.")
}

rm(dt.files)
